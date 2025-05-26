use super::{Ty,Value};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    Return(Value),
}

impl Instruction {
    pub fn ty(&self) -> Option<&Ty> {
        match self {
            Self::Return(_value) => None,
        }
    }

    pub fn produces_value(&self) -> bool {
        self.ty().is_some()
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Return(value) => write!(f, "return {}", value),
        }
    }
}