use super::{
    Constant,
    InstructionRef,
    Function,
    Ty
};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    Constant(Constant),
    Instruction(InstructionRef, String),
}

impl Value {
    pub fn constant(value: Constant) -> Self {
        Self::Constant(value)
    }

    pub fn ty<'a>(&'a self, function: &'a Function) -> Ty {
        match self {
            Self::Constant(c) => c.ty().clone(),
            Self::Instruction(instruction_ref, _name) => function.get_instruction(*instruction_ref).ty(function).unwrap(),
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
