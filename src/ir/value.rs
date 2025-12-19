use super::{
    InstructionRef,
    Function,
    Ty
};
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value(InstructionRef, String);

impl Value {
    pub fn new(instruction_ref: InstructionRef, name: String) -> Self {
        Self(instruction_ref, name)
    }

    pub fn instruction_ref(&self) -> InstructionRef {
        self.0
    }

    pub fn name(&self) -> &str {
        &self.1
    }

    pub fn ty<'a>(&'a self, function: &'a Function) -> Ty {
        function.get_instruction(self.0).ty(function).unwrap()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "%{}", self.1)
    }
}
