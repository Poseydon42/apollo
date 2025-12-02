use super::BasicBlock;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Function {
    /// Name of the function
    pub name: String,

    /// The basic blocks that make up the function. By convention, the name of the entry basic block is the same as the function name.
    pub basic_blocks: HashSet<BasicBlock>,
}

impl Function {
    pub fn new(name: String, basic_blocks: HashSet<BasicBlock>) -> Self {
        Self {
            name,
            basic_blocks,
        }
    }
}
