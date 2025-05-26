use super::{Instruction, Value};
use super::instruction_list::*;

#[derive(Debug)]
pub struct BasicBlock {
    name: String,
    instructions: InstructionList,
    next_unnamed_value_id: u32,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: InstructionList::new(),
            next_unnamed_value_id: 0,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn get_instruction(&self, instruction_ref: InstructionRef) -> &Instruction {
        self.instructions.get(instruction_ref)
    }

    pub fn get_instruction_mut(&mut self, instruction_ref: InstructionRef) -> &mut Instruction {
        self.instructions.get_mut(instruction_ref)
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.instructions()
    }

    pub fn append_instruction(&mut self, instruction: Instruction) -> (InstructionRef, Option<Value>) {
        let produces_value = instruction.produces_value();
        let instruction_ref = self.instructions.append(instruction);
        let value = match produces_value {
            true => {
                let id = self.next_unnamed_value_id;
                self.next_unnamed_value_id += 1;
                Some(Value::Instruction(instruction_ref, format!("{}", id)))
            }
            false => None,
        };
        (instruction_ref, value)
    }
}
