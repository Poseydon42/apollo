use super::{BasicBlock,Ty,Value};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    Add(Value, Value),
    Sub(Value, Value),

    Allocate(Ty),
    Load { location: Value, ty: Ty},
    Store { value: Value, location: Value},

    Return(Value),
}

impl Instruction {
    pub fn ty<'a>(&'a self, bb: &'a BasicBlock) -> Option<Ty> {
        match self {
            Self::Add(lhs, rhs) |
            Self::Sub(lhs, rhs) => {
                assert_eq!(lhs.ty(bb), rhs.ty(bb), "Operands of an arithmetic operation must have the same type");
                Some(lhs.ty(bb).clone())
            }

            Self::Allocate(..) => Some(Ty::Ptr),
            Self::Load { location: _, ty } => Some(ty.clone()),
            Self::Store { .. } => None,

            Self::Return(..) => None,
        }
    }

    pub fn produces_value(&self) -> bool {
        match self {
            Self::Store{..}  |
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

            Self::Allocate(ty) => write!(f, "allocate {}", ty),
            Self::Load { location: value, ty } => write!(f, "load {} as {}", value, ty),
            Self::Store { value, location: dst } => write!(f, "store {} to {}", value, dst),

            Self::Return(value) => write!(f, "return {}", value),
        }
    }
}