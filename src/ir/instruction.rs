use super::Value;
use std::fmt::*;

#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Value),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Return(value) => write!(f, "return {}", value),
        }
    }
}