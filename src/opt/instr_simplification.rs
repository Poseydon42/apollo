use crate::ir::*;
use super::OptPass;

pub struct InstrSimplification;

impl InstrSimplification {
    pub fn new() -> Self {
        Self
    }

    fn simplify_phi(&self, function: &mut Function, instr: InstructionRef) {
        let Instruction::Phi { incoming, .. } = function.get_instruction(instr) else { unreachable!() };

        if incoming.len() == 1 {
            let value = incoming[0].0.clone();
            function.replace_all_uses_with(function.get_value(instr).unwrap(), value);
        }
    }
}

impl OptPass for InstrSimplification {
    fn run(&mut self, function: &mut Function) {
        let instructions: Vec<_> = function
            .get_basic_blocks()
            .flat_map(|bb| function.get_instructions_in_basic_block(bb.name()))
            .map(|(instr_ref, _)| instr_ref)
            .collect();
        for instr_ref in instructions {
            let instr = function.get_instruction(instr_ref);
            match instr {
                Instruction::Phi { .. } => self.simplify_phi(function, instr_ref),
                _ => { },
            }
        }
    }
}
