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

    // NOTE: we iterate in reverse orderw and map on encountering a use since restrictions on
    //       register classes are defined by the uses, not the defs.
    for (idx, instruction) in order.iter().enumerate().rev() {
        let instruction = dag.get(*instruction);
        assert!(
            !instruction.is_dead() || instruction.id() == *order.last().unwrap(),
            "Dead instruction ({}) should have been removed before register allocation", instruction.to_string());

        for (input, value) in instruction.inputs().enumerate() {
            if !is_register_type(dag.get_value_type(*value)) {
                continue;
            }

            if value_map.contains_key(value) {
                let reg = value_map[value];
                assert!(
                    instruction.is_input_allowed_in_register(input, reg),
                    "We assigned a register to value {} that is not allowed in input {} of instruction {}",
                    value, input, instruction.to_string()
                );
                continue;
            }

            if let Some(output) = instruction.opcode().get_native().is_input_overwritten_by_output(input) {
                // If the output is placed in the same register as the current input, we can assign the same register.
                assert!(
                    live_ranges[&value].1 == idx,
                    "We do not yet support reusing input registers that are alised with outputs if the input is used after the instruction"
                );
                let output_value = instruction.get_output(output);
                let reg = value_map[&output_value];
                value_map.insert(*value, reg);
                continue;
            }

            let reg = free_registers.iter().filter(|reg| instruction.is_input_allowed_in_register(input, **reg)).cloned().next();

            if let Some(reg) = reg {
                value_map.insert(*value, reg);
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