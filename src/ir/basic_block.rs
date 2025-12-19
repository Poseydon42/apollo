use super::{
    InstructionRef,
    instruction_list::InstructionList,
};
use std::borrow::Borrow;
use std::hash::{
    Hash,
    Hasher,
};

#[derive(Debug)]
pub struct BasicBlock {
    name: String,
    instructions: InstructionList,    
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: InstructionList::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn instructions(&self) -> impl Iterator<Item = &InstructionRef> {
        self.instructions.instructions()
    }

    pub fn append_instruction(&mut self, instruction_ref: InstructionRef) {
        self.instructions.append(instruction_ref);
    }
}

impl PartialEq for BasicBlock {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for BasicBlock {}

impl Hash for BasicBlock {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Borrow<str> for BasicBlock {
    fn borrow(&self) -> &str {
        &self.name
    }
}