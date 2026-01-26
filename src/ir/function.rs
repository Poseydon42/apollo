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

    /// Name of the entry basic block
    entry_basic_block_name: String,

    /// The ID given to the next unnamed value
    next_unnamed_value_id: u32,
}

impl Function {
    pub fn new(name: String) -> Self {
        Self {
            name: name.clone(),
            instructions: Vec::new(),
            values: HashMap::new(),
            basic_blocks: OrderSet::new(),
            entry_basic_block_name: name,
            next_unnamed_value_id: 1,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn print(&self) {
        for bb in self.get_basic_blocks() {
            println!("{}:", bb.name());
            for (instruction_ref, instruction) in self.get_instructions_in_basic_block(bb.name()) {
                if instruction.produces_value() {
                    println!("  {} = {}", self.get_value(instruction_ref).unwrap(), instruction);
                } else {
                    println!("  {}", instruction);
            }
            }
        }
    }

    pub fn get_instruction(&self, instruction_ref: InstructionRef) -> &Instruction {
        &self.instructions[instruction_ref.0]
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

    pub fn entry_basic_block_name(&self) -> &str {
        &self.entry_basic_block_name
    }

    pub fn update_entry_basic_block_name(&mut self, bb_name: String) {
        assert!(self.basic_blocks.contains(bb_name.as_str()), "Cannot set entry basic block to non-existent basic block");
        self.entry_basic_block_name = bb_name;
    }

    pub fn get_entry_basic_block(&self) -> &BasicBlock {
        self.get_basic_block(self.entry_basic_block_name())
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
        bb.name() == self.entry_basic_block_name()
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

    pub fn get_direct_predecessors(&self, bb: &str) -> Vec<&str> {
        self.get_basic_blocks()
            .filter_map(|candidate_bb| {
                let terminator = self.get_instruction(self.get_terminator_of_basic_block(candidate_bb.name()));
                let targets = terminator.target_basic_blocks();
                if targets.contains(&bb) {
                    Some(candidate_bb.name())
                } else {
                    None
                }
            })
            .collect()
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

    pub fn calculate_reachability(&self, from: &str, to: &str) -> Reachability {
        let mut reachable = HashMap::new();
        let mut worklist = vec![(from, Reachability::Unconditional)];
        while let Some((current_bb, current_reachability)) = worklist.pop() {
            // NOTE: the extra check of reachability kind ensures that we do not prematurely return
            //       with an overly pessimistic result.
            if current_bb == to && current_reachability == Reachability::Unconditional {
                return Reachability::Unconditional;
            }

            if let Some(existing_reachabiilty) = reachable.get(current_bb)
                && current_reachability <= *existing_reachabiilty {
                continue;
            }
            reachable.insert(current_bb, current_reachability);

            let reachability_on_termination = match self.get_instruction(self.get_terminator_of_basic_block(current_bb)) {
                Instruction::Branch { .. } => Reachability::Conditional,
                Instruction::Jump { .. } => Reachability::Unconditional,
                Instruction::Return { .. } => Reachability::Unreachable,
                _ => unreachable!("Terminator instruction of a BB should be a control flow instruction"),
            };
            let descendant_reachability = reachability_on_termination.min(current_reachability);
            for descendant_bb in self.get_direct_descendants(current_bb) {
                worklist.push((descendant_bb, descendant_reachability));
            }
        }

        *reachable.get(to).unwrap_or(&Reachability::Unreachable)
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

    /// Moves the instruction pointed to by `instruction_ref` to a new position, indicated by `bb` and `insert_after`.
    /// If `insert_after` is `None`, the instruction is moved to the beginning of the basic block `bb`.
    pub fn move_instruction(&mut self, instruction_ref: InstructionRef, bb: &str, insert_after: Option<InstructionRef>) {
        let original_bb = self.get_basic_block_of_instruction(instruction_ref);
        if original_bb.name() != bb && self.get_instruction(instruction_ref).produces_value() {
            for user in self.get_value_users(&self.get_value(instruction_ref).unwrap()) {
                let user_bb = self.get_basic_block_of_instruction(user).name();
                let reachability = self.calculate_reachability(bb, user_bb);
                match reachability {
                    Reachability::Unconditional |
                    Reachability::Conditional if self.get_instruction(user).is_phi() => {},

                    _ => {
                        let instruction = self.get_instruction(instruction_ref);
                        panic!("Cannot move instruction {instruction} to basic block `{bb}` as it is used in basic block `{user_bb}` with insufficient reachability")
                    }
                }
            }
        }

        let original_bb = original_bb.name().to_owned();
        self.basic_blocks.get_full_mut2(original_bb.as_str()).unwrap().1.remove_instruction(instruction_ref);

        self.basic_blocks.get_full_mut2(bb).unwrap().1.insert_instruction(instruction_ref, insert_after);
    }

    pub fn replace_instruction(&mut self, instruction: InstructionRef, new_instruction: Instruction) {
        let bb = self.get_basic_block_of_instruction(instruction);
        if self.get_terminator_of_basic_block(bb.name()) == instruction {
            // NOTE: replacing a terminator instruction requires more care to ensure the CFG is updated properly,
            //       and that we don't have instructions in other BBs referencing values that are not dominating them.
            let bb_name = bb.name().to_string();
            self.replace_terminator_instruction(&bb_name, new_instruction);
        } else {
            assert!(self.get_value(instruction).map(|value| value.ty(self)) == new_instruction.ty(self), "Cannot replace instruction with one that produces a different type");
            self.instructions[instruction.0] = new_instruction;
        }
    }

    pub fn replace_all_uses_with(&mut self, old_value: Value, new_value: Value) {
        let users: Vec<_> = self.get_value_users(&old_value).collect();
        for user in users {
            let instruction = self.get_instruction(user).clone();
            let new_instruction = instruction.replace_operand(&old_value, &new_value);
            self.replace_instruction(user, new_instruction);
        }
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

        if bb_name == self.entry_basic_block_name() {
            panic!("Removing the entry basic block is not supported");
        }

        // FIXME: should we deal with updating jumps to this BB, as well as removing/updating values that reference instructions in this BB?
    }

    fn replace_terminator_instruction(&mut self, bb: &str, new_instruction: Instruction) {
        let old_instruction = self.get_instruction(self.get_terminator_of_basic_block(bb));
        let old_targets = old_instruction.target_basic_blocks();
        let new_targets = new_instruction.target_basic_blocks();

        let removed_targets: Vec<&str> = old_targets.iter().filter(|t| !new_targets.contains(t)).cloned().collect();
        let added_targets: Vec<&str> = new_targets.iter().filter(|t| !old_targets.contains(t)).cloned().collect();

        if added_targets.len() > 0 {
            todo!("Replacing terminator with one that jumps to more basic blocks is not yet supported");
        }

        // FIXME: this should be replaced with proper dominance recalculation
        let mut phis_to_modify = vec![];
        for target in removed_targets {
            for (instr_ref, _) in self.get_instructions_in_basic_block(target) {
                let uses_value_from_bb = self.get_instruction(instr_ref)
                    .operands()
                    .iter()
                    .any(|operand| {
                        self.get_basic_block_of_instruction(operand.instruction_ref()) == self.get_basic_block(bb)
                    });
                if !uses_value_from_bb {
                    continue;
                }

                if matches!(self.get_instruction(instr_ref), Instruction::Phi { .. }) {
                    phis_to_modify.push(instr_ref);
                }
            }
        }

        for phi in phis_to_modify {
            let Instruction::Phi { incoming, ty } = self.get_instruction(phi).clone() else { unreachable!() };
            let new_incoming: Vec<_> = incoming.into_iter()
                .filter(|(_, incoming_bb)| incoming_bb != bb)
                .collect();
            let new_phi = Instruction::Phi { incoming: new_incoming, ty };
            self.replace_instruction(phi, new_phi);
        }

        let replaced_instr_ref = self.get_terminator_of_basic_block(bb);
        self.instructions[replaced_instr_ref.0] = new_instruction;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reachability {
    Unreachable = 0,
    Conditional = 1,
    Unconditional = 2,
}
