use super::Function;

pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    pub fn functions_mut(&mut self) -> &mut [Function] {
        &mut self.functions
    }
}
