use crate::ast::*;
use crate::ir;
use ordermap::OrderSet;
use std::collections::HashMap;

pub struct IRGenerator {
    module: ir::Module,
    function_context: Option<FunctionContext>,
}

struct FunctionContext {
    variables: HashMap<String, ir::Value>,
    basic_blocks: OrderSet<ir::BasicBlock>,
    current_bb: ir::BasicBlock,
    next_if_bb_id: u32,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            module: ir::Module::new(),
            function_context: None,
        }
    }

    pub fn generate(mut self, ast_module: &ast::Module) -> ir::Module {
        Visitor::visit_module(&mut self, ast_module);
        self.module
    }

    fn get_function_context(&self) -> &FunctionContext {
        self.function_context.as_ref().expect("Current function context must be set")
    }

    fn get_function_context_mut(&mut self) -> &mut FunctionContext {
        self.function_context.as_mut().expect("Current function context must be set")
    }

    fn get_bb_mut(&mut self) -> &mut ir::BasicBlock {
        &mut self.get_function_context_mut().current_bb
    }
}

impl Visitor<'_, Option<ir::Value>> for IRGenerator {
    fn visit_func_decl(&mut self, func_decl: &ast::FunctionDecl, _node: &ast::Decl) -> Option<ir::Value> {
        assert!(self.function_context.is_none(), "Function declaration within function declaration is not supported");

        self.function_context = Some(FunctionContext {
            variables: HashMap::new(),
            basic_blocks: OrderSet::new(),
            current_bb: ir::BasicBlock::new(func_decl.name.text().to_owned()),
            next_if_bb_id: 0,
        });

        let return_value = self.visit_expr(&func_decl.body);
        match return_value {
            Some(val) => {
                let ret_instruction = ir::Instruction::Return(val);
                self.get_bb_mut().append_instruction(ret_instruction);

                // Trick here: we can't not have a BB, but since we're done with this function anyway,
                // we can just swap the current BB with an empty one and insert the completed BB into the function's set of BBs
                let new_bb = ir::BasicBlock::new("".to_owned());
                let completed_bb = std::mem::replace(self.get_bb_mut(), new_bb);
                self.get_function_context_mut().basic_blocks.insert(completed_bb);
            }
            None => {}
        }

        let function_context = self.function_context.take().expect("Function context must be set");

        let basic_blocks = function_context.basic_blocks;
        let function = ir::Function::new(func_decl.name.text().to_owned(), basic_blocks);
        self.module.add_function(function);

        None
    }

    fn visit_variable_decl(&mut self, variable: &'_ VariableDecl, _node: &'_ Decl) -> Option<ir::Value> {
        assert!(self.function_context.is_some(), "Variable declaration outside of function is not supported");

        let ty = variable.ty.resolved.as_ref().expect("Variable declaration must have a resolved AST type during IR generation");
        let allocate = ir::Instruction::Allocate(lower_type(ty));
        let ptr = self.get_bb_mut().append_named_instruction(allocate, variable.name.text().to_owned()).1.unwrap();

        let init = self.visit_expr(&variable.init).expect("Variable initialization expression must produce a node");
        let store = ir::Instruction::Store { value: init.clone(), location: ptr.clone() };
        self.get_bb_mut().append_instruction(store);
        self.function_context.as_mut().unwrap().variables.insert(variable.name.text().to_owned(), ptr.clone());

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
        let ptr = self
            .get_function_context()
            .variables
            .get(expr.name.text())
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

    fn visit_if(&mut self, expr: &'_ If, _node: &'_ Expr) -> Option<ir::Value> {
        let condition = self.visit_expr(&*expr.condition).expect("Condition expression of if expression must produce a node");

        let then_bb_name = format!("then.{}", self.get_function_context().next_if_bb_id);
        let else_bb_name = format!("else.{}", self.get_function_context().next_if_bb_id);
        let joining_bb_name = format!("post_if.{}", self.get_function_context().next_if_bb_id);
        self.get_function_context_mut().next_if_bb_id += 1;
        let branch_instruction = ir::Instruction::Branch {
            condition: condition.clone(),
            then_bb: then_bb_name.clone(),
            else_bb: else_bb_name.clone(),
        };
        self.get_bb_mut().append_instruction(branch_instruction);

        let then_bb = ir::BasicBlock::new(then_bb_name.clone());
        let if_bb = std::mem::replace(&mut self.get_function_context_mut().current_bb, then_bb);
        self.get_function_context_mut().basic_blocks.insert(if_bb);
        let then_value = self.visit_expr(&*expr.then_branch).expect("Then branch of if expression must produce a node");
        let then_value_ty = then_value.ty(&self.get_function_context().current_bb);
        let jump_to_joining = ir::Instruction::Jump(joining_bb_name.clone());
        self.get_bb_mut().append_instruction(jump_to_joining);

        let else_bb = ir::BasicBlock::new(else_bb_name.clone());
        let then_bb = std::mem::replace(&mut self.get_function_context_mut().current_bb, else_bb);
        self.get_function_context_mut().basic_blocks.insert(then_bb);
        let else_value = self.visit_expr(&*expr.else_branch).expect("Else branch of if expression must produce a node");
        let jump_to_joining = ir::Instruction::Jump(joining_bb_name.clone());
        self.get_bb_mut().append_instruction(jump_to_joining);

        let joining_bb = ir::BasicBlock::new(joining_bb_name.clone());
        let else_bb = std::mem::replace(&mut self.get_function_context_mut().current_bb, joining_bb);
        self.get_function_context_mut().basic_blocks.insert(else_bb);
        
        let phi = ir::Instruction::Phi {
            incoming: vec![ (then_value, then_bb_name), (else_value, else_bb_name) ],
            ty: then_value_ty
        };
        self.get_bb_mut().append_instruction(phi).1
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