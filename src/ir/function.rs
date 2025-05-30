use super::BasicBlock;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub code: BasicBlock,
}

impl Function {
    pub fn new(name: String, code: BasicBlock) -> Self {
        Self {
            name,
            code,
        }
    }
}
