mod cfg_simplification;
mod constant_folding;
mod dead_code_elimination;
mod instr_simplification;

use crate::ir::{
    Function,
};

trait OptPass {
    fn run(&mut self, function: &mut Function);
}

pub fn run_opt_pipeline(function: &mut Function) {
    let mut passes: Vec<Box<dyn OptPass>> = vec![
        Box::new(constant_folding::ConstantFolding::new()),
        Box::new(dead_code_elimination::DeadCodeElimination::new()),
        Box::new(cfg_simplification::CFGSimplification::new()),
        Box::new(instr_simplification::InstrSimplification::new()),
        Box::new(dead_code_elimination::DeadCodeElimination::new()),
    ];

    for pass in passes.iter_mut() {
        pass.run(function);
    }
}
