use super::{
    Constant,
    Function,
    Ty,
    Value
};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    Const(Constant),
    Arg { index: u32, ty: Ty },

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
    pub fn operands(&self) -> Vec<&Value> {
        match self {
            Self::Const(_) => vec![],
            Self::Arg { .. } => vec![],

            Self::Add(lhs, rhs) |
            Self::Sub(lhs, rhs) => vec![lhs, rhs],

            Self::Allocate(_) => vec![],
            Self::Load { location, .. } => vec![location],
            Self::Store { value, location } => vec![value, location],

            Self::Phi { incoming, .. } => incoming.iter().map(|(value, _)| value).collect(),

            Self::Jump(_) => vec![],
            Self::Branch { condition, .. } => vec![condition],
            Self::Return(value) => vec![value],
        }
    }

    pub fn replace_operand(&self, old: &Value, new: &Value) -> Instruction {
        match self {
            Self::Const(c) => Self::Const(c.clone()),
            Self::Arg { index, ty } => Self::Arg { index: *index, ty: ty.clone() },

            Self::Add(lhs, rhs) => Self::Add(
                if lhs == old { new.clone() } else { lhs.clone() },
                if rhs == old { new.clone() } else { rhs.clone() },
            ),

            Self::Sub(lhs, rhs) => Self::Sub(
                if lhs == old { new.clone() } else { lhs.clone() },
                if rhs == old { new.clone() } else { rhs.clone() },
            ),

            Self::Allocate(ty) => Self::Allocate(ty.clone()),
            Self::Load { location, ty } => Self::Load {
                location: if location == old { new.clone() } else { location.clone() },
                ty: ty.clone(),
            },
            Self::Store { value, location } => Self::Store {
                value: if value == old { new.clone() } else { value.clone() },
                location: if location == old { new.clone() } else { location.clone() },
            },

            Self::Phi { incoming, ty } => {
                let new_incoming: Vec<_> = incoming
                    .iter()
                    .map(|(value, bb_name)| {
                        let new_value = if value == old { new.clone() } else { value.clone() };
                        (new_value, bb_name.clone())
                    })
                    .collect();
                Self::Phi {
                    incoming: new_incoming,
                    ty: ty.clone(),
                }
            }

            Self::Jump(label) => Self::Jump(label.clone()),
            Self::Branch { condition, then_bb, else_bb } => Self::Branch {
                condition: if condition == old { new.clone() } else { condition.clone() },
                then_bb: then_bb.clone(),
                else_bb: else_bb.clone(),
            },
            Self::Return(value) => Self::Return(
                if value == old { new.clone() } else { value.clone() }
            ),
        }
    }

    pub fn ty<'a>(&'a self, function: &'a Function) -> Option<Ty> {
        match self {
            Self::Const(constant) => Some(constant.ty().clone()),
            Self::Arg { ty, .. } => Some(ty.clone()),

            Self::Add(lhs, rhs) |
            Self::Sub(lhs, rhs) => {
                assert_eq!(lhs.ty(function), rhs.ty(function), "Operands of an arithmetic operation must have the same type");
                Some(lhs.ty(function).clone())
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

    pub fn is_phi(&self) -> bool {
        matches!(self, Self::Phi { .. })
    }

    pub fn is_control_flow(&self) -> bool {
        match self {
            Self::Jump(..)   |
            Self::Branch{..} |
            Self::Return(..) => true,

            _ => false,
        }
    }

    pub fn target_basic_blocks(&self) -> Vec<&str> {
        match self {
            Self::Jump(label) => vec![label.as_str()],
            Self::Branch { then_bb, else_bb, .. } => vec![then_bb.as_str(), else_bb.as_str()],
            Self::Return(..) => vec![],
            _ => panic!("This instruction does not jump to any basic blocks"),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Const(c) => write!(f, "const {}", c),
            Self::Arg { index, ty } => write!(f, "arg #{} as {}", index, ty),

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