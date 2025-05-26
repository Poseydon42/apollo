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

    pub fn instructions(&self) -> impl Iterator<Item = (InstructionRef, &Instruction)> {
        InstructionIter {
            list: self,
            next: self.instructions.first_index()
        }
    }

    pub fn append(&mut self, instruction: Instruction) -> InstructionRef {
        InstructionRef(self.instructions.insert_last(instruction))
    }
}

pub struct InstructionIter<'a> {
    list: &'a InstructionList,
    next: Index,
}

impl<'a> Iterator for InstructionIter<'a> {
    type Item = (InstructionRef, &'a Instruction);

    fn next(&mut self) -> Option<Self::Item> {
        if self.next.is_none() {
            return None;
        }
        let instruction_ref = InstructionRef(self.next);
        let instruction = self.list.get(instruction_ref);
        self.next = self.list.instructions.next_index(self.next);
        Some((instruction_ref, instruction))
    }
}