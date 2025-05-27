use super::dag::*;
use super::isa::*;
use std::collections::{HashMap, HashSet};

pub struct RegisterAllocationResult<I: ISA> {
    pub value_map: HashMap<Value, I::Register>,
}

pub fn allocate_registers<I: ISA>(dag: &DAG<I>, order: &[NodeId], isa: &I) -> RegisterAllocationResult<I> {
    let mut value_map = HashMap::new();
    let mut free_registers : HashSet<_> = isa.get_usable_registers().into_iter().collect();
    let live_ranges = calculate_live_ranges(dag, order);
    
    println!("Live ranges:");
    for (value, (start, end)) in &live_ranges {
        println!("\tValue {} live from {} to {}", value, start, end);
    }

    for (idx, instruction) in order.iter().enumerate() {
        let instruction = dag.get(*instruction);
        assert!(
            !instruction.is_dead() || instruction.id() == *order.last().unwrap(),
            "Dead instruction ({}) should have been removed before register allocation", instruction.to_string());

        for i in 0..instruction.output_count() {
            if !is_register_type(instruction.get_output_type(i)) {
                continue;
            }

            let value = dag.get_value(instruction.id(), i);
            assert!(!value_map.contains_key(&value), "Value {:?} already has a register allocated", value);

            if let Some(input_id) = instruction.opcode().get_native().is_output_alised_with_input(i) {
                let input = instruction.get_input(input_id);
                // If the input value is not used after this instruction and the output register is aliased with it, we can reuse it.
                if live_ranges[&input].1 == idx {
                    value_map.insert(value, value_map[&input]);
                    continue;
                }
            }

            let reg = free_registers.iter().filter(|reg|
                dag.uses(value).all(|(node, port)| dag.get(*node).is_input_allowed_in_register(*port, reg))
            ).cloned().next();

            if let Some(reg) = reg {
                value_map.insert(value, reg);
                free_registers.remove(&reg);
            } else {
                panic!("No free allowed register available for value {}", value);
            }
        }
    }

    RegisterAllocationResult {
        value_map
    }
}

fn is_register_type<I: ISA>(ty: &OutputType<I>) -> bool {
    match ty {
        OutputType::Native(_) => true,

        _ => false,
    }
}

fn calculate_live_ranges<I: ISA>(dag: &DAG<I>, order: &[NodeId]) -> HashMap<Value, (usize, usize)> {
    let mut live_ranges = HashMap::new();

    for (idx, id) in order.iter().enumerate() {
        let node = dag.get(*id);
        node.inputs()
            .cloned()
            .chain(node.outputs())
            .for_each(
            |value| {
                if !is_register_type(dag.get_value_type(value)) {
                    return;
                }
                let range = live_ranges.entry(value).or_insert((idx, idx));
                range.0 = range.0.min(idx);
                range.1 = range.1.max(idx);
            }
        );
    }

    live_ranges
}