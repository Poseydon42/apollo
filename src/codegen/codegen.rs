use super::dag::*;
use super::ir_lowering::*;
use super::isa::*;
use super::NativeFunction;
use super::opcode::*;
use super::register_allocator::*;
use crate::ir;
use std::collections::HashSet;

pub fn codegen_function<I: ISA>(function: &ir::Function, isa: I, debug_dump: bool) -> Option<NativeFunction> {
    let IRLoweringResult { mut dag } = lower_basic_block(function, &function.code, &isa);

    if debug_dump {
        println!("Initial DAG:");
        for node in dag.nodes() {
            println!("\t{}", node.to_string());
        }
    }

    let mut selection_queue = create_selection_queue(&dag);
    while !selection_queue.is_empty() {
        let node_id = selection_queue.pop().unwrap();
        if dag.get(node_id).is_native() {
            continue;
        }
        println!("\tProcessing node: {:?}", node_id);
        isa.select_instruction(&mut dag, node_id);
    }

    if debug_dump {
        println!("Final DAG:");
        for node in dag.nodes() {
            if node.is_dead() && node.id() != dag.root().expect("DAG must contain a root after instruction selection") {
                continue;
            }
            println!("\t{}", node.to_string());
        }
    }

    let schedule = schedule(&dag);
    if debug_dump {
        println!("Scheduled instructions:");
        for node in schedule.iter() {
            println!("\t{}", dag.get(*node).to_string());
        }
    }

    let register_allocation = allocate_registers(&dag, &schedule, &isa);
    if debug_dump {
        println!("Register allocation:");
        for (value, reg) in register_allocation.value_map.iter() {
            println!("\t{} -> {}", value, reg.to_string());
        }
    }

    let native_instructions: Vec<_> = schedule.iter()
        .filter_map(|node| {
            let instruction = dag.get(*node);
            if instruction.is_generic() {
                return None;
            }
            let native_instruction = isa.build_native_instruction(&dag, instruction, &register_allocation);
            if native_instruction.is_none() {
                panic!("Failed to build native instruction for {}", instruction.to_string());
            }
            Some(native_instruction.unwrap())
        })
        .collect();
    if debug_dump {
        println!("Native instructions:");
        for instruction in native_instructions.iter() {
            println!("\t{}", instruction.to_string());
        }
    }

    Some(NativeFunction {
        name: function.name.clone(),
        lines: native_instructions.iter().map(|i| i.to_string()).collect(),
    })
}

fn create_selection_queue<I: ISA>(dag: &DAG<I>) -> Vec<NodeId> {
    let mut queue = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![dag.root().expect("DAG must contain a root before instruction selection")];

    while let Some(node_id) = stack.pop() {
        if visited.contains(&node_id) {
            continue;
        }
        visited.insert(node_id);
        if does_node_need_selection(dag, node_id) {
            queue.push(node_id);
        }

        for input in dag.get(node_id).inputs() {
            if !visited.contains(&input.node()) {
                stack.push(input.node());
            }
        }
    }

    queue.reverse();
    queue
}

fn schedule<I: ISA>(dag: &DAG<I>) -> Vec<NodeId> {
    let mut result = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![dag.root().expect("DAG must contain a root before instruction selection")];
    
    while let Some(node) = stack.pop() {
        visited.insert(node);
        result.push(node);

        // NOTE: we want the first input to be the furthest away from the current node
        // just in case it is a control flow edge.
        for input in dag.get(node).inputs() {
            if !visited.contains(&input.node()) {
                stack.push(input.node());
            }
        }
    }

    result.reverse();
    result
}

fn does_node_need_selection<I: ISA>(dag: &DAG<I>, node: NodeId) -> bool {
    match dag.get(node).opcode() {
        Opcode::Generic(opcode) => match opcode {
            GenericOpcode::Enter        |
            GenericOpcode::Constant(..) |
            GenericOpcode::Register(..) => false,

            _ => true,
        }
        Opcode::Native(_) => false,
    }
}
