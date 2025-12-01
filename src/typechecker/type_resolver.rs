use crate::ast::*;

pub fn resolve_type(raw_type: &str) -> Option<ResolvedType> {
    match raw_type {
        "bool" => Some(ResolvedType::Bool),
        "i32" => Some(ResolvedType::Int),
        
        _ => None,
    }
}