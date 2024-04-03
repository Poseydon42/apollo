use super::dag::*;
use super::isa::*;
use std::collections::{HashMap, HashSet};

pub struct RegisterAllocationResult<I: ISA> {
    pub value_map: HashMap<Value, I::Register>,
}

pub fn allocate_registers<I: ISA>(dag: &DAG<I>, order: &[NodeId], isa: &I) -> RegisterAllocationResult<I> {
    let mut value_map = HashMap::new();
    let mut free_registers : HashSet<_> = isa.get_usable_registers().into_iter().collect();

    for instruction in order {
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
