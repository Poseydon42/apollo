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

    fn get_known_constant(&self, value: &Value) -> Option<Constant> {
        self.known_constants.get(value).cloned()
    }
}

impl OptPass for ConstantFolding {
    fn run(&mut self, function: &mut Function) {
        for bb in function.get_owned_basic_block_names() {
            for (instr_ref, _) in function.get_instructions_in_basic_block(&bb) {
                let value = function.get_value(instr_ref);
                let Some(value) = value else { continue; };

                match self.try_evaluate(&value, function) {
                    Some(constant) => {
                        self.known_constants.insert(value, constant.clone());
                    }
                    None => {},
                }
            }
        }

        for (value, constant) in &self.known_constants {
            let instr_ref = value.instruction_ref();
            let instr = function.get_instruction_mut(instr_ref);
            *instr = Instruction::Const(constant.clone());
        }
    }
}
