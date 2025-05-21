use super::Ty;
use super::Constant;

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
