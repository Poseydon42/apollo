use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FunctionContext {
    return_type: ResolvedType,
    variables: HashMap<String, ResolvedType>,
}

impl FunctionContext {
    pub fn new(return_type: ResolvedType) -> Self {
        Self {
            return_type,
            variables: HashMap::new(),
        }
    }

    pub fn return_type(&self) -> &ResolvedType {
        &self.return_type
    }

    pub fn add_variable(&mut self, name: String, ty: ResolvedType) {
        self.variables.insert(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<&ResolvedType> {
        self.variables.get(name)
    }
}
