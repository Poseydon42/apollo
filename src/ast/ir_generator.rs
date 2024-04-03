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

        walk_func_decl(func, self);

        let context = self.context.take().unwrap();
        let function = ir::Function {
            name: func.name.text().to_owned(),
            code: context.bb,
        };
        self.module.add_function(function);
        self.context = None; // Reset context after function declaration

        None
    }

    fn visit_integer_literal_expr(&mut self, expr: &'_ ast::Expr) -> Option<ir::Value> {
        assert!(matches!(expr.data, ast::ExprKind::IntegerLiteral));

        // FIXME: this is so, so bad. We should use some sort of binary representation of an
        //        "abstract typed value" instead of relying on Rust's integer parsing functionality
        let value: i32 = expr.span.text().parse().expect("Value of an integer literal must be a valid 32 bit signed integer");
        let value = ir::Value::immediate_int(value);
        Some(value)
    }

    fn visit_binary_expr(&mut self, _lhs: &'_ ast::Expr, _op: ast::BinaryOp, _rhs: &'_ ast::Expr, _expr: &'_ ast::Expr) -> Option<ir::Value> {
        unimplemented!("Binary expressions are not yet implemented in IR generation");
    }

    fn visit_return_stmt(&mut self, value: &'_ ast::Expr, _stmt: &'_ ast::Stmt) -> Option<ir::Value> {
        let value = self.visit_expr(value).expect("Visited expression must produce a node");
        
        let ret = ir::Instruction::Return(value);
        self.get_bb_mut().add_instruction(ret);

        None
    }
}
