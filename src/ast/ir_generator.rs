use crate::ast::*;
use crate::ir;
use std::collections::HashMap;

pub struct IRGenerator {
    module: ir::Module,
    function: Option<ir::Function>,
    variables: HashMap<String, ir::Value>,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            module: ir::Module::new(),
            function: None,
            variables: HashMap::new(),
        }
    }

    pub fn generate(mut self, ast_module: &ast::Module) -> ir::Module {
        Visitor::visit_module(&mut self, ast_module);
        self.module
    }

    fn get_bb_mut(&mut self) -> &mut ir::BasicBlock {
        &mut self.function.as_mut().expect("Function must be set").code
    }
}

impl Visitor<'_, Option<ir::Value>> for IRGenerator {
    fn visit_func_decl(&mut self, func_decl: &ast::FunctionDecl, _node: &ast::Decl) -> Option<ir::Value> {
        assert!(self.function.is_none(), "Function declaration within function declaration is not supported");

        let bb = ir::BasicBlock::new(func_decl.name.text().to_owned());
        let function = ir::Function::new(func_decl.name.text().to_owned(), bb);
        self.function = Some(function);
        self.variables.clear();

        let return_value = self.visit_expr(&func_decl.body);
        match return_value {
            Some(val) => {
                let ret_instruction = ir::Instruction::Return(val);
                self.get_bb_mut().append_instruction(ret_instruction);
            }
            None => {}
        }

        let function = self.function.take().unwrap();
        self.module.add_function(function);
        self.function = None;

        None
    }

    fn visit_variable_decl(&mut self, variable: &'_ VariableDecl, _node: &'_ Decl) -> Option<ir::Value> {
        assert!(self.function.is_some(), "Variable declaration outside of function is not supported");

        let ty = variable.ty.resolved.as_ref().expect("Variable declaration must have a resolved AST type during IR generation");
        let allocate = ir::Instruction::Allocate(lower_type(ty));
        let ptr = self.get_bb_mut().append_named_instruction(allocate, variable.name.text().to_owned()).1.unwrap();

        let init = self.visit_expr(&variable.init).expect("Variable initialization expression must produce a node");
        let store = ir::Instruction::Store { value: init.clone(), location: ptr.clone() };
        self.get_bb_mut().append_instruction(store);
        self.variables.insert(variable.name.text().to_owned(), ptr.clone());

        // NOTE: variable declarations produce a value corresponding to their initialized value
        Some(init)
    }

    fn visit_block(&mut self, block: &'_ Block, _node: &'_ Expr) -> Option<ir::Value> {
        for expr in &block.ignored_exprs {
            self.visit_expr(expr);
        }

        if let Some(last_expr) = &block.last_expr {
            self.visit_expr(&*last_expr)
        } else {
            None
        }
    }

    fn visit_integer_literal(&mut self, expr: &'_ ast::IntegerLiteral, _node: &'_ ast::Expr) -> Option<ir::Value> {
        // FIXME: this is so, so bad. We should use some sort of binary representation of an
        //        "abstract typed value" instead of relying on Rust's integer parsing functionality
        let value: i32 = expr.raw_value.text().parse().expect("Value of an integer literal must be a valid 32 bit signed integer");
        let value = ir::Value::constant(ir::Constant::int(value));
        Some(value)
    }

    fn visit_boolean_literal(&mut self, expr: &'_ ast::BooleanLiteral, _node: &'_ ast::Expr) -> Option<ir::Value> {
        let value = ir::Value::constant(ir::Constant::bool(expr.value));
        Some(value)
    }

    fn visit_variable_reference(&mut self, expr: &'_ VariableReference, _node: &'_ Expr) -> Option<ir::Value> {
        let ptr = self.variables.get(expr.name.text())
            .expect("Variable reference must refer to a previously declared variable");
        let load = ir::Instruction::Load { 
            location: ptr.clone(), 
            ty: lower_type(expr.ty.as_ref().expect("Variable reference nodes must have a resolved type during IR generation"))
        };
        self.get_bb_mut().append_instruction(load).1
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr, _node: &Expr) -> Option<ir::Value> {
        let lhs = self.visit_expr(&*expr.lhs).expect("Left-hand side of binary expression must produce a node");
        let rhs = self.visit_expr(&*expr.rhs).expect("Right-hand side of binary expression must produce a node");
        let instruction = match expr.op {
            ast::BinaryOp::Add => ir::Instruction::Add(lhs, rhs),
            ast::BinaryOp::Sub => ir::Instruction::Sub(lhs, rhs),
        };
        self.get_bb_mut().append_instruction(instruction).1
    }

    fn visit_return(&mut self, expr: &Return, _node: &Expr) -> Option<ir::Value> {
        let value = self.visit_expr(&*expr.value).expect("Return expression must produce a node");
        let instruction = ir::Instruction::Return(value);
        self.get_bb_mut().append_instruction(instruction);
        None
    }
}

fn lower_type(ty: &ResolvedType) -> ir::Ty {
    match ty {
        ResolvedType::Empty => unreachable!("Cannot lower () type to IR type"),
        ResolvedType::Bool => ir::Ty::Bool,
        ResolvedType::Int => ir::Ty::Int,
    }
}