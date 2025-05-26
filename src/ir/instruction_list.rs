use super::Instruction;
use index_list::*;

#[derive(Debug)]
pub(super) struct InstructionList {
    instructions: IndexList<Instruction>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstructionRef(Index);

impl InstructionList {
    pub fn new() -> Self {
        Self {
            instructions: IndexList::new(),
        }
    }

    pub fn get(&self, instruction: InstructionRef) -> &Instruction {
        self.instructions.get(instruction.0).expect("InstructionRef is invalid")
    }

    pub fn get_mut(&mut self, instruction: InstructionRef) -> &mut Instruction {
        self.instructions.get_mut(instruction.0).expect("InstructionRef is invalid")
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }

    pub fn append(&mut self, instruction: Instruction) -> InstructionRef {
        InstructionRef(self.instructions.insert_last(instruction))
    }
}
