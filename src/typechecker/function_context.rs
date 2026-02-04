use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FunctionContext {
    return_type: ResolvedType,
    explicit_return_guaranteed: bool,
    variables: HashMap<String, ResolvedType>,
}

impl FunctionContext {
    pub fn new(return_type: ResolvedType) -> Self {
        Self {
            return_type,
            explicit_return_guaranteed: false,
            variables: HashMap::new(),
        }
    }

    pub fn return_type(&self) -> &ResolvedType {
        &self.return_type
    }

    pub fn is_explicit_return_guaranteed(&self) -> bool {
        self.explicit_return_guaranteed
    }

    pub fn set_explicit_return_guaranteed(&mut self) {
        self.explicit_return_guaranteed = true;
    }

    pub fn add_variable(&mut self, name: String, ty: ResolvedType) {
        self.variables.insert(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<&ResolvedType> {
        self.variables.get(name)
    }
}
