use std::fmt::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedType {
    Empty,
    Bool,
    Int,
}

impl ResolvedType {
    pub fn to_string(&self) -> String {
        match self {
            ResolvedType::Empty => "()".to_string(),
            ResolvedType::Bool => "bool".to_string(),
            ResolvedType::Int => "i32".to_string(),
        }
    }
}

