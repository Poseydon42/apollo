use super::{BasicBlock,Constant,InstructionRef,Ty};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Constant(Constant),
    Instruction(InstructionRef, String),
}

impl Value {
    pub fn constant(value: Constant) -> Self {
        Self::Constant(value)
    }

    pub fn ty<'a>(&'a self, bb: &'a BasicBlock) -> &'a Ty {
        match self {
            Self::Constant(c) => c.ty(),
            Self::Instruction(instruction_ref, _name) => bb.get_instruction(*instruction_ref).ty(bb).unwrap(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Instruction(_ref, name) => write!(f, "%{}", name),
        }
    }
}
