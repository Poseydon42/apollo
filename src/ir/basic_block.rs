use super::{
    Instruction,
    instruction_list::*,
    Value
};
use std::borrow::Borrow;
use std::collections::{
    HashMap,
};
use std::hash::{
    Hash,
    Hasher,
};

#[derive(Debug)]
pub struct BasicBlock {
    name: String,
    instructions: InstructionList,
    values: HashMap<InstructionRef, Value>,
    next_unnamed_value_id: u32,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: InstructionList::new(),
            values: HashMap::new(),
            next_unnamed_value_id: 1,
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

    pub fn instructions(&self) -> impl Iterator<Item = (InstructionRef, &Instruction)> {
        self.instructions.instructions()
    }

    pub fn get_value(&self, instruction_ref: InstructionRef) -> Option<Value> {
        self.values.get(&instruction_ref).cloned()
    }

    pub fn append_instruction(&mut self, instruction: Instruction) -> (InstructionRef, Option<Value>) {
        let name = format!("{}", self.next_unnamed_value_id);
        self.next_unnamed_value_id += 1;
        self.append_named_instruction(instruction, name)
    }

    pub fn append_named_instruction(&mut self, instruction: Instruction, name: String) -> (InstructionRef, Option<Value>) {
        let produces_value = instruction.produces_value();
        let instruction_ref = self.instructions.append(instruction);
        let value = match produces_value {
            true => {
                let value = Value::Instruction(instruction_ref, name);
                self.values.insert(instruction_ref, value.clone());
                Some(value)
            }
            false => None,
        };
        (instruction_ref, value)
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