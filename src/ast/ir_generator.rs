use crate::ast::*;
use crate::ir;

pub struct IRGenerator {
    module: ir::Module,
    context: Option<FunctionContext>,
}

struct FunctionContext {
    bb: ir::BasicBlock,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            module: ir::Module::new(),
            context: None
        }
    }

    pub fn generate(mut self, ast_module: &ast::Module) -> ir::Module {
        Visitor::visit_module(&mut self, ast_module);
        self.module
    }

    fn get_bb_mut(&mut self) -> &mut ir::BasicBlock {
        &mut self.context.as_mut().expect("Context must be set").bb
    }
}

impl Visitor<'_, Option<ir::Value>> for IRGenerator {
    fn visit_func_decl(&mut self, func: &ast::FunctionDecl, _node: &ast::Decl) -> Option<ir::Value> {
        assert!(self.context.is_none(), "Function declaration within function declaration is not supported");

        let bb = ir::BasicBlock::new(func.name.text().to_owned());
        let context = FunctionContext {
            bb,
        };
        self.context = Some(context);

        func.body.iter().for_each(|stmt| { 
            self.visit_stmt(stmt);
        });

        let context = self.context.take().unwrap();
        let function = ir::Function {
            name: func.name.text().to_owned(),
            code: context.bb,
        };
        self.module.add_function(function);
        self.context = None; // Reset context after function declaration

        None
    }

    fn visit_integer_literal_expr(&mut self, expr: &'_ ast::IntegerLiteralExpr, _node: &'_ ast::Expr) -> Option<ir::Value> {
        // FIXME: this is so, so bad. We should use some sort of binary representation of an
        //        "abstract typed value" instead of relying on Rust's integer parsing functionality
        let value: i32 = expr.raw_value.text().parse().expect("Value of an integer literal must be a valid 32 bit signed integer");
        let value = ir::Value::constant(ir::Constant::int(value));
        Some(value)
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

    fn visit_return_stmt(&mut self, value: &'_ ast::Expr, _stmt: &'_ ast::Stmt) -> Option<ir::Value> {
        let value = self.visit_expr(value).expect("Visited expression must produce a node");
        
        let ret = ir::Instruction::Return(value);
        self.get_bb_mut().append_instruction(ret);

        None
    }
}
