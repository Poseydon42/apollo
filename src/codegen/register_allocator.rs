use super::dag::*;
use super::isa::*;
use super::opcode::*;
use std::collections::{HashMap, HashSet};

pub struct RegisterAllocationResult<I: ISA> {
    pub value_map: HashMap<Value, I::Register>,
}

pub fn allocate_registers<I: ISA>(dag: &mut DAG<I>, order: &mut Vec<NodeId>, isa: &I) -> RegisterAllocationResult<I> {
    let mut value_map = HashMap::new();
    let mut free_registers : HashSet<_> = isa.get_usable_registers().into_iter().collect();
    let live_ranges = calculate_live_ranges(dag, order);
    
    println!("Live ranges:");
    for (value, (start, end)) in &live_ranges {
        println!("\tValue {} live from {} to {}", value, start, end);
    }

    // NOTE: we iterate in reverse order and map on encountering a use since restrictions on
    //       register classes are defined by the uses, not the defs.
    let mut register_copies = Vec::new();
    for (idx, instruction) in order.iter().enumerate().rev() {
        let instruction = dag.get(*instruction);
        assert!(
            !instruction.is_dead() || instruction.id() == *order.last().unwrap(),
            "Dead instruction ({}) should have been removed before register allocation", instruction.to_string());

        for (input, value) in instruction.inputs().enumerate() {
            if !is_register_type(dag.get_value_type(*value)) {
                continue;
            }

            let node = dag.get(value.node());

            // Register nodes can be assigned their register directly. This also allows us to use special registers like RBP/RSP that
            // would not be normally allocated to any virtual register.
            if node.opcode().is_register() {
                value_map.insert(*value, *node.opcode().get_register());
                continue;
            }
            if node.opcode().is_constant() {
                // The ISA will have either lowered constants into native instructions or will use as immediate values in instructions.
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

            if let Some(output) = is_input_overwritten_by_output(instruction.opcode(), input) {
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

        // We need to do one last minute correction: if we have an instruction that takes a fixed register as input,
        // and then overwrites that input, we need to insert a copy to a new register before the instruction.
        for input in 0..instruction.inputs().count() {
            let Some(overwriting_output) = instruction.opcode().get_native().is_input_overwritten_by_output(input) else {
                continue;
            };

            let input_value = instruction.get_input(input);
            let input_node = dag.get(input_value.node());
            if !input_node.opcode().is_register() {
                continue;
            }

            let assigned_reg = value_map[&instruction.get_output(overwriting_output)];
            register_copies.push((input_value, instruction.id(), input, assigned_reg));
        }
    }

    for (value, instruction, port, reg) in &register_copies {
        // Copy the value
        let copied_value = isa.insert_register_copy(dag, *value);
 
        // Use the copied value as input
        dag.set_input(*instruction, *port, copied_value);

        // Assign it a register
        value_map.insert(copied_value, *reg);
 
        // Insert the instruction into the schedule right before the instruction that needs it
        let insert_pos = order.iter().position(|id| *id == *instruction).unwrap();
        order.insert(insert_pos, copied_value.node());
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

fn is_input_overwritten_by_output<I: ISA>(opcode: &Opcode<I>, input: usize) -> Option<usize> {
    match opcode {
        Opcode::Native(opcode) => opcode.is_input_overwritten_by_output(input),
        _ => None,
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