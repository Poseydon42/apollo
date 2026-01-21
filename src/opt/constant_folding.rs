use crate::ir::{
    Constant,
    Function,
    Instruction,
    Ty,
    Value,
};
use super::OptPass;
use std::collections::HashMap;

pub struct ConstantFolding {
    known_constants: HashMap<Value, Constant>,
}

impl ConstantFolding {
    pub fn new() -> Self {
        Self {
            known_constants: HashMap::new(),
        }
    }

    fn try_evaluate(&self, value: &Value, function: &Function) -> Option<Constant> {
        let instr = function.get_instruction(value.instruction_ref());

        match instr {
            Instruction::Const(constant) => Some(constant.clone()),

            Instruction::Add(lhs, rhs) => {
                let lhs = self.get_known_constant(lhs)?;
                let rhs = self.get_known_constant(rhs)?;

                if lhs.ty() != rhs.ty() || lhs.ty() != &Ty::Int {
                    None
                } else {
                    // FIXME: double check that this is *exactly* what we want to do here
                    let add = (lhs.bits_as_u64() as u32).wrapping_add(rhs.bits_as_u64() as u32);
                    Some(Constant::int(add as i32))
                }
            }

            _ => None,
        }
    }

    fn try_fold_branch(&self, instr: &Instruction) -> Option<Instruction> {
        match instr {
            Instruction::Branch { condition, then_bb, else_bb } => {
                let Some(condition) = self.get_known_constant(condition) else { return None; };
                let condition = condition.bits_as_u64() != 0;
                let target_bb = if condition { then_bb } else { else_bb }.clone();
                Some(Instruction::Jump(target_bb))
            }

            _ => unreachable!(),
        }
    }

    fn get_known_constant(&self, value: &Value) -> Option<Constant> {
        self.known_constants.get(value).cloned()
    }
}

impl OptPass for ConstantFolding {
    fn run(&mut self, function: &mut Function) {
        let mut replaced_jumps = vec![];

        for bb in function.get_owned_basic_block_names() {
            for (instr_ref, _) in function.get_instructions_in_basic_block(&bb) {
                let instr = function.get_instruction(instr_ref);
                let value = function.get_value(instr_ref);

                if let Some(value) = value {
                    match self.try_evaluate(&value, function) {
                        Some(constant) => {
                            self.known_constants.insert(value, constant.clone());
                        }
                        None => {},
                    }
                } else if matches!(instr, Instruction::Branch { .. }) {
                    if let Some(new_instr) = self.try_fold_branch(instr) {
                        replaced_jumps.push((instr_ref, new_instr));
                    }
                }
            }
        }

        for (value, constant) in &self.known_constants {
            function.replace_instruction(value.instruction_ref(), Instruction::Const(constant.clone()))
        }
        for (instr_ref, new_instr) in replaced_jumps {
            function.replace_instruction(instr_ref, new_instr);
        }
    }
}
