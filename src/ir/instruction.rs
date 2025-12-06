use super::{BasicBlock,Ty,Value};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    Add(Value, Value),
    Sub(Value, Value),

    Allocate(Ty),
    Load { location: Value, ty: Ty},
    Store { value: Value, location: Value},

    Phi { incoming: Vec<(Value, String)>, ty: Ty },

    Jump(String),
    Branch { condition: Value, then_bb: String, else_bb: String },
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

            Self::Phi { ty, .. } => Some(ty.clone()),

            Self::Jump(..)   |
            Self::Branch{..} |
            Self::Return(..) => None,
        }
    }

    pub fn produces_value(&self) -> bool {
        match self {
            Self::Store{..}  |
            Self::Jump(..)   |
            Self::Branch{..} |
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

            Self::Phi { incoming, ty } => {
                write!(f, "phi {} [", ty)?;
                for (i, (value, bb_name)) in incoming.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} -> {}", bb_name, value)?;
                }
                write!(f, "]")
            }

            Self::Jump(label) => write!(f, "jump {}", label),
            Self::Branch { condition, then_bb, else_bb } => write!(f, "branch {}, {}, {}", condition, then_bb, else_bb),
            Self::Return(value) => write!(f, "return {}", value),
        }
    }
}