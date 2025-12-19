use super::InstructionRef;
use index_list::*;

#[derive(Debug)]
pub(super) struct InstructionList {
    instructions: IndexList<InstructionRef>,
}

impl InstructionList {
    pub fn new() -> Self {
        Self {
            instructions: IndexList::new(),
        }
    }

    pub fn instructions(&self) -> impl Iterator<Item = &InstructionRef> {
        self.instructions.iter()
    }

    pub fn append(&mut self, instruction: InstructionRef) {
        self.instructions.insert_last(instruction);
    }
}
