use crate::ir::*;
use super::OptPass;

pub struct CFGSimplification;

impl CFGSimplification {
    pub fn new() -> Self {
        Self
    }

    /// Merges the two basic blocks `first` and `second` into a single basic block.
    /// Second must be the only successor of first, and first must be the only predecessor of second.
    fn merge_basic_blocks(&self, function: &mut Function, first: &str, second: &str) {
        // NOTE: we should compute and store the list of predescessor before moving the instructions, because in the process of that
        //       we will leave one of the basic blocks of the function empty, putting it into an invalid state (as a basic block must
        //       have a terminator instruction). Calling any functions on an IR function in an invalid state is not a good idea,
        //       so we precompute all we need first.
        let predescessors: Vec<_> = function.get_direct_predecessors(first).iter().map(|s| s.to_string()).collect();
        
        // 1. Move instructions from first to second
        let instructions_to_move: Vec<_> = function.get_instructions_in_basic_block(first)
            .filter(|(_, instr)| !instr.is_control_flow())
            .map(|(instr_ref, _)| instr_ref)
            .rev()
            .collect();
        for instr_ref in instructions_to_move {
            function.move_instruction(instr_ref, second, None);
        }

        // 2. Redirect predecessors of first to second
        for predescessor in predescessors {
            let terminator_ref = function.get_terminator_of_basic_block(&predescessor);
            let terminator = function.get_instruction(terminator_ref);
            let patched_terminator = self.patch_branch_instruction(&terminator, first, second);
            function.replace_instruction(terminator_ref, patched_terminator);
        }
        
        // 3. Remove first from function
        if function.entry_basic_block_name() == first {
            function.update_entry_basic_block_name(second.to_string());
        }
        function.remove_basic_block(first);
    }

    fn patch_branch_instruction(&self, instruction: &Instruction, old_target: &str, new_target: &str) -> Instruction {
        match instruction {
            Instruction::Branch { condition, then_bb, else_bb } => {
                let new_then_bb = if then_bb == old_target { new_target.to_string() } else { then_bb.clone() };
                let new_else_bb = if else_bb == old_target { new_target.to_string() } else { else_bb.clone() };
                Instruction::Branch {
                    condition: condition.clone(),
                    then_bb: new_then_bb,
                    else_bb: new_else_bb,
                }
            },
            Instruction::Jump(label) => {
                assert!(label == old_target, "Trying to patch a jump instruction that does not target the old target");
                Instruction::Jump(new_target.to_string())
            }
            _ => unreachable!("Trying to patch a non-branch instruction"),
        }
    }

    fn try_merge(&self, function: &mut Function, bb: &str) -> bool {
        let terminator = function.get_instruction(function.get_terminator_of_basic_block(&bb));
        let Instruction::Jump(target_bb) = terminator else {
            return false;
        };
        let target_bb = target_bb.clone();

        let predecessors: Vec<_> = function.get_direct_predecessors(&target_bb).iter().map(|s| s.to_owned()).collect();
        if predecessors.len() == 1 && predecessors[0] == bb {
            self.merge_basic_blocks(function, &bb, &target_bb);
            true
        } else {
            false
        }
    }

    fn try_remove(&self, function: &mut Function, bb: &str) -> bool {
        if function.get_direct_predecessors(bb).is_empty() && function.get_entry_basic_block().name() != bb {
            // Need to patch all the phis to remove the incoming values from this basic block.
            let descendants: Vec<_> = function
                .get_direct_descendants(bb)
                .iter()
                .map(|s| s.to_string())
                .collect();
            for descendant in &descendants {
                let phis: Vec<_> = function
                    .get_instructions_in_basic_block(descendant)
                    .filter(|(_, instr)| instr.is_phi())
                    .map(|x| x.0)
                    .collect();
                for phi_ref in phis {
                    if let Instruction::Phi { incoming, ty } = function.get_instruction(phi_ref) {
                        let new_incoming: Vec<_> = incoming
                            .iter()
                            .filter(|(_, pred_bb)| pred_bb != bb)
                            .cloned()
                            .collect();
                        let new_phi = Instruction::Phi {
                            incoming: new_incoming,
                            ty: ty.clone(),
                        };
                        function.replace_instruction(phi_ref, new_phi);
                    }
                }
            }

            function.remove_basic_block(bb);
            true
        } else {
            false
        }
    }

    fn perform_iteration(&self, function: &mut Function) -> bool {
        let mut changed = false;
        
        for bb in function.get_owned_basic_block_names() {
            changed |= self.try_remove(function, &bb);
        }
        
        for bb in function.get_owned_basic_block_names() {
            changed |= self.try_merge(function, &bb);
        }

        changed
    }
}

impl OptPass for CFGSimplification {
    fn run(&mut self, function: &mut Function) {
        loop {
            let changed = self.perform_iteration(function);
            if !changed {
                break;
            }
        }
    }
}
