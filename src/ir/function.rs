use super::BasicBlock;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub code: BasicBlock,
}
