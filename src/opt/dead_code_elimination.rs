use crate::ir::{
    Function,
};
use std::collections::HashSet;
use super::OptPass;

pub struct DeadCodeElimination;

impl DeadCodeElimination {
    pub fn new() -> Self {
        Self
    }
}

impl OptPass for DeadCodeElimination {
    fn run(&mut self, function: &mut Function) {
        let mut removal_list = HashSet::new();

        for bb in function.get_owned_basic_block_names().into_iter().rev() {
            for (instr_ref, _) in function.get_instructions_in_basic_block(&bb).rev() {
                let value = function.get_value(instr_ref);
                match value {
                    Some(value) => {
                        if function.get_value_users(&value).all(|instr| removal_list.contains(&instr)) {
                            removal_list.insert(instr_ref);
                        }
                    },

                    // NOTE: instruction that do not produce values are not dealt with yet as they have other side effects
                    // such as control flow changes or memory writes
                    None => continue,
                }
            }
        }

        for instr_ref in removal_list {
            function.remove_instruction(instr_ref);
        }
    }
}
