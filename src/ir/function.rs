use super::{
    BasicBlock,
    Instruction,
    Value,
};
use std::collections::HashMap;
use ordermap::{OrderSet, set::MutableValues};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstructionRef(usize);

#[derive(Debug)]
pub struct Function {
    /// Name of the function
    name: String,

    /// Instructions within the function
    instructions: Vec<Instruction>,

    /// The list of values defined within the function
    values: HashMap<InstructionRef, Value>,

    /// The basic blocks that make up the function. By convention, the name of the entry basic block is the same as the function name.
    basic_blocks: OrderSet<BasicBlock>,

    /// The ID given to the next unnamed value
    next_unnamed_value_id: u32,
}

impl Function {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            values: HashMap::new(),
            basic_blocks: OrderSet::new(),
            next_unnamed_value_id: 1,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn get_instruction(&self, instruction_ref: InstructionRef) -> &Instruction {
        &self.instructions[instruction_ref.0]
    }

    pub fn get_instruction_mut(&mut self, instruction_ref: InstructionRef) -> &mut Instruction {
        &mut self.instructions[instruction_ref.0]
    }

    pub fn get_instructions_in_basic_block(&self, bb: &str) -> impl DoubleEndedIterator<Item = (InstructionRef, &Instruction)> {
        self.basic_blocks
            .get(bb)
            .unwrap()
            .instructions()
            .map(|instruction_ref| (*instruction_ref, self.get_instruction(*instruction_ref)))
    }

    pub fn get_terminator_of_basic_block(&self, bb: &str) -> InstructionRef {
        let bb = self.basic_blocks.get(bb).unwrap();
        *bb.instructions().last().expect("Basic block has no instructions")
    }

    pub fn get_value(&self, instruction_ref: InstructionRef) -> Option<Value> {
        self.values.get(&instruction_ref).cloned()
    }

    pub fn get_basic_block(&self, bb: &str) -> &BasicBlock {
        self.basic_blocks.get(bb).unwrap()
    }

    pub fn get_entry_basic_block(&self) -> &BasicBlock {
        self.get_basic_block(self.name())
    }

    pub fn get_basic_blocks(&self) -> impl DoubleEndedIterator<Item = &BasicBlock> {
        self.basic_blocks.iter()
    }

    pub fn get_basic_block_names(&self) -> impl DoubleEndedIterator<Item = &str> {
        self.basic_blocks.iter().map(|bb| bb.name())
    }

    pub fn get_owned_basic_block_names(&self) -> Vec<String> {
        self.basic_blocks.iter().map(|bb| bb.name().to_string()).collect()
    }

    pub fn get_basic_block_of_instruction(&self, instruction: InstructionRef) -> &BasicBlock {
        for bb in self.get_basic_blocks() {
            if bb.instructions().any(|instr_ref| *instr_ref == instruction) {
                return bb;
            }
        }
        panic!("Instruction {:?} does not belong to any basic block", instruction);
    }

    pub fn get_value_users(&self, value: &Value) -> impl Iterator<Item = InstructionRef> {
        self.instructions
            .iter()
            .enumerate()
            .filter_map(move |(instr_ref, instr)| {
                if instr.operands().contains(&value) {
                    Some(InstructionRef(instr_ref))
                } else {
                    None
                }
            })
    }

    pub fn is_entry_bb(&self, bb: &BasicBlock) -> bool {
        assert!(self.basic_blocks.contains(bb.name()));
        bb.name() == self.name()
    }

    pub fn is_terminating_bb(&self, bb: &BasicBlock) -> bool {
        assert!(self.basic_blocks.contains(bb.name()));
        let last_instruction_ref = bb.instructions().last();
        match last_instruction_ref {
            Some(instruction_ref) => {
                let instruction = self.get_instruction(*instruction_ref);
                matches!(instruction, Instruction::Return(..))
            }
            None => false,
        }
    }

    pub fn get_direct_descendants(&self, bb: &str) -> Vec<&str> {
        let terminator = self.get_terminator_of_basic_block(bb);
        match self.get_instruction(terminator) {
            Instruction::Jump(target) => vec![target.as_str()],
            Instruction::Branch { then_bb, else_bb, .. } => vec![then_bb.as_str(), else_bb.as_str()],
            
            _ => vec![],
        }
    }

    pub fn is_instruction_value_used_in_external_basic_blocks(&self, instruction: InstructionRef) -> bool {
        let own_bb = self.get_basic_block_of_instruction(instruction);
        for bb in self.get_basic_blocks() {
            if bb.name() == own_bb.name() {
                continue;
            }
            for instr_ref in bb.instructions() {
                let instr = self.get_instruction(*instr_ref);
                for operand in instr.operands() {
                    if operand.instruction_ref() == instruction {
                        return true;
                    }
                }
            }
        }
        false
    }

    pub fn add_basic_block(&mut self, bb: BasicBlock) {
        assert!(self.basic_blocks.insert(bb), "Cannot add basic block with duplicate name");
    }

    pub fn append_instruction(&mut self, bb: &str, instruction: Instruction) -> (InstructionRef, Option<Value>) {
        let name = if instruction.produces_value() {
            let name = format!("{}", self.next_unnamed_value_id);
            self.next_unnamed_value_id += 1;
            name
        } else {
            String::new()
        };
        self.append_named_instruction(bb, instruction, name)
    }

    pub fn append_named_instruction(&mut self, bb: &str, instruction: Instruction, name: String) -> (InstructionRef, Option<Value>) {
        assert!(self.basic_blocks.contains(bb), "Trying to append instruction to a non-existent BB '{}'", bb);
        let produces_value = instruction.produces_value();
        let instruction_ref = self.instructions.len();
        self.instructions.push(instruction);
        self.basic_blocks.get_full_mut2(bb).unwrap().1.append_instruction(InstructionRef(instruction_ref));
        let value = match produces_value {
            true => {
                let value = Value::new(InstructionRef(instruction_ref), name);
                self.values.insert(InstructionRef(instruction_ref), value.clone());
                Some(value)
            }
            false => None,
        };
        (InstructionRef(instruction_ref), value)
    }

    pub fn remove_instruction(&mut self, instruction_ref: InstructionRef) {
        // Remove from basic block
        let bb = self.get_basic_block_of_instruction(instruction_ref).name().to_string();
        self.basic_blocks.get_full_mut2(bb.as_str()).unwrap().1.remove_instruction(instruction_ref);

        // Remove from values map
        self.values.remove(&instruction_ref);
    }

    pub fn remove_basic_block(&mut self, bb_name: &str) {
        assert!(self.basic_blocks.contains(bb_name), "Trying to remove non-existent basic block '{}'", bb_name);

        let instructions: Vec<_> = self.basic_blocks.get(bb_name).unwrap().instructions().cloned().collect();
        for instruction_ref in &instructions {
            self.remove_instruction(*instruction_ref);
        }

        self.basic_blocks.remove(bb_name);

        // FIXME: should we deal with updating jumps to this BB, as well as removing/updating values that reference instructions in this BB?
    }
}
