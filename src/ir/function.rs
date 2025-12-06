use super::BasicBlock;
use ordermap::OrderSet;

#[derive(Debug)]
pub struct Function {
    /// Name of the function
    pub name: String,

    /// The basic blocks that make up the function. By convention, the name of the entry basic block is the same as the function name.
    pub basic_blocks: OrderSet<BasicBlock>,
}

impl Function {
    pub fn new(name: String, basic_blocks: OrderSet<BasicBlock>) -> Self {
        Self {
            name,
            basic_blocks,
        }
    }
}
