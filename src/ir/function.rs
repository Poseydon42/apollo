use super::BasicBlock;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub code: BasicBlock,
}
