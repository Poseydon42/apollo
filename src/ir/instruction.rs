use super::{BasicBlock,Ty,Value};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    Add(Value, Value),
    Sub(Value, Value),

    Return(Value),
}

impl Instruction {
    pub fn ty<'a>(&'a self, bb: &'a BasicBlock) -> Option<&'a Ty> {
        match self {
            Self::Add(lhs, rhs) |
            Self::Sub(lhs, rhs) => {
                assert_eq!(lhs.ty(bb), rhs.ty(bb), "Operands of an arithmetic operation must have the same type");
                Some(lhs.ty(bb))
            }

            Self::Return(_value) => None,
        }
    }

    pub fn produces_value(&self) -> bool {
        match self {
            Self::Return(..) => false,

            _ => true,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Add(lhs, rhs) => write!(f, "add {}, {}", lhs, rhs),
            Self::Sub(lhs, rhs) => write!(f, "sub {}, {}", lhs, rhs),

            Self::Return(value) => write!(f, "return {}", value),
        }
    }
}