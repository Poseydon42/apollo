use super::dag::*;
use super::ir_lowering::*;
use super::isa::*;
use super::NativeFunction;
use super::opcode::*;
use super::register_allocator::*;
use crate::ir;
use std::collections::{
    BinaryHeap,
    HashSet,
};
use std::cmp::Ordering;

pub fn codegen_function<I: ISA>(function: &ir::Function, isa: I, debug_dump: bool) -> Option<NativeFunction> {
    let mut lines = Vec::new();
    for bb in function.basic_blocks.iter() {
        if debug_dump {
            println!("Codegen for basic block: {}", bb.name());
        }
        let bb_lines = codegen_basic_block(function, bb, &isa, debug_dump);
        match bb_lines {
            Some(bb_lines) => {
                lines.push(format!("{}:", bb.name()));
                lines.extend(bb_lines.into_iter().map(|line| format!("  {}", line)));
            }
            None => {
                return None;
            }
        }

        if debug_dump {
            println!("\n\n\n");
        }
    }
    
    Some(NativeFunction {
        name: function.name.clone(),
        lines,
    })
}

pub fn codegen_basic_block<I: ISA>(function: &ir::Function, bb: &ir::BasicBlock, isa: &I, debug_dump: bool) -> Option<Vec<String>> {
    let IRLoweringResult { mut dag } = lower_basic_block(function, bb, isa);

    if debug_dump {
        println!("Initial DAG:");
        dump_graph(&dag);
    }

    // NOTE: we should *always* maintain the invariant that a node is selected before any
    //       of its inputs are selected, so that the uses have an easier time integrating some
    //       of their inputs into themselves during instruction selection.
    let selection_queue = create_selection_queue(&dag);
    for node_id in selection_queue.iter().rev() {
        if dag.get(*node_id).is_native() {
            continue;
        }
        isa.select_instruction(&mut dag, *node_id);
    }

    if debug_dump {
        println!("Final DAG:");
        dump_graph(&dag);
    }

    let mut schedule = schedule(&dag);
    if debug_dump {
        println!("Scheduled instructions:");
        for node in schedule.iter() {
            println!("\t{}", dag.get(*node).to_string());
        }
    }

    let register_allocation = allocate_registers(&mut dag, &mut schedule, &isa);
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
    let mut native_instructions: Vec<_> = isa
        .generate_prologue()
        .into_iter()
        .chain(native_instructions.into_iter())
        .collect();
    // NOTE: we should insert the epilogue before ret
    native_instructions.splice(
        native_instructions.len() - 1..native_instructions.len() - 1,
         isa.generate_epilogue().into_iter()
    );
    if debug_dump {
        println!("Native instructions:");
        for instruction in native_instructions.iter() {
            println!("\t{}", instruction.to_string());
        }
    }

    Some(native_instructions.iter().map(|i| i.to_string()).collect())
}

fn create_selection_queue<I: ISA>(dag: &DAG<I>) -> Vec<NodeId> {
    schedule(dag).iter().filter(|node| does_node_need_selection(dag, **node)).cloned().collect()
}

fn schedule<I: ISA>(dag: &DAG<I>) -> Vec<NodeId> {
    #[derive(PartialEq, Eq, Debug)]
    struct NodeEntry {
        node: NodeId,
        has_ctrl: bool,
    }
    impl PartialOrd for NodeEntry {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            if self.has_ctrl && !other.has_ctrl {
                // We want nodes with control flow to be scheduled last, which would
                // put them *first* in the final ordering (since it's reversed at the end).
                Some(Ordering::Less)
            }
            else if !self.has_ctrl && other.has_ctrl {
                Some(Ordering::Greater)
            }
            else {
                Some(Ordering::Equal)
            }
        }
    }
    impl Ord for NodeEntry {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).unwrap()
        }
    }

    let mut available = BinaryHeap::new();
    let mut scheduled = HashSet::new();
    let mut result = Vec::new();
    
    let root = dag.root().expect("DAG must be complete when scheduling");
    available.push(NodeEntry { node: root, has_ctrl: true });
    while let Some(node) = available.pop() {
        result.push(node.node);
        scheduled.insert(node.node);

        for input in dag.get(node.node).inputs() {
            let input_node = input.node();
            if scheduled.contains(&input_node) {
                continue;
            }

            let all_uses_scheduled =
                dag.get(input_node)
                .all_uses()
                .all(|(use_node, _)| scheduled.contains(&use_node));
            if !all_uses_scheduled {
                continue;
            }

            // NOTE: this will not be true for the last node of the DAG control flow (i.e. ret/branch), but we will have processed
            //       it by now anyway as it's the root of the DAG, which is added to the priority queue anyway.
            let has_ctrl = dag.get(input_node).outputs().any(|value| dag.get_value_type(value).is_control());
            let entry = NodeEntry { node: input_node, has_ctrl };
            if !available.iter().any(|e| e.node == input_node) {
                available.push(entry);
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

fn dump_graph<I: ISA>(dag: &DAG<I>) {
    for node in dag.nodes() {
        if node.is_dead() && node.id() != dag.root().expect("DAG must contain a root after instruction selection") {
            continue;
        }
        println!("\t{}", node);
    }
}