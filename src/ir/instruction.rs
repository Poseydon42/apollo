use super::Value;

#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Value),
}