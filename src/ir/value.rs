use super::{Constant,Ty};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Constant(Constant),
}

impl Value {
    pub fn constant(value: Constant) -> Self {
        Self::Constant(value)
    }

    pub fn ty(&self) -> &Ty {
        match self {
            Self::Constant(c) => c.ty(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
        }
    }
}
