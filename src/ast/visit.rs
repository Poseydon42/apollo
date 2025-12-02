use crate::ast::*;

pub trait Visitor<'ast, ReturnType = ()> : Sized where
    ReturnType : Default {
    fn visit_module(&mut self, module: &'ast Module) -> ReturnType {
        for decl in &module.decls {
            self.visit_decl(decl);
        }
        Default::default()
    }

    fn visit_decl(&mut self, decl: &'ast Decl) -> ReturnType {
        match &decl.data {
            DeclKind::Function(func_decl) => self.visit_func_decl(&func_decl, decl),
            DeclKind::Variable(variable_decl) => self.visit_variable_decl(&variable_decl, decl),
        }
    }

    fn visit_func_decl(&mut self, func: &'ast FunctionDecl, _node: &'ast Decl) -> ReturnType {
        self.visit_expr(&func.body);
        Default::default()
    }

    fn visit_variable_decl(&mut self, variable: &'ast VariableDecl, _node: &'ast Decl) -> ReturnType {
        self.visit_expr(&variable.init);
        Default::default()
    }

    fn visit_expr(&mut self, expr: &'ast Expr) -> ReturnType {
        match &expr.data {
            ExprKind::Block(block_expr) => self.visit_block(block_expr, expr),
            ExprKind::Decl(decl) => self.visit_decl(&*decl),
            ExprKind::IntegerLiteral(literal_expr) => self.visit_integer_literal(literal_expr, expr),
            ExprKind::BooleanLiteral(boolean_literal_expr) => self.visit_boolean_literal(boolean_literal_expr, expr),
            ExprKind::VariableReference(variable_reference_expr) => self.visit_variable_reference(variable_reference_expr, expr),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(binary_expr, expr),
            ExprKind::If(if_expr) => self.visit_if(if_expr, expr),
            ExprKind::Return(return_expr) => self.visit_return(return_expr, expr),
        }
    }

    fn visit_block(&mut self, block_expr: &'ast Block, _node: &'ast Expr) -> ReturnType {
        for stmt in &block_expr.ignored_exprs {
            self.visit_expr(stmt);
        }
        if let Some(last_expr) = &block_expr.last_expr {
            self.visit_expr(&*last_expr);
        }
        Default::default()
    }

    fn visit_integer_literal(&mut self, _expr: &'ast IntegerLiteral, _node: &'ast Expr) -> ReturnType {
        Default::default()
    }

    fn visit_boolean_literal(&mut self, _expr: &'ast BooleanLiteral, _node: &'ast Expr) -> ReturnType {
        Default::default()
    }

    fn visit_variable_reference(&mut self, _expr: &'ast VariableReference, _node: &'ast Expr) -> ReturnType {
        Default::default()
    }

    fn visit_binary_expr(&mut self, expr: &'ast BinaryExpr, _node: &'ast Expr) -> ReturnType {
        self.visit_expr(&*expr.lhs);
        self.visit_expr(&*expr.rhs);
        Default::default()
    }

    fn visit_if(&mut self, expr: &'ast If, _node: &'ast Expr) -> ReturnType {
        self.visit_expr(&*expr.condition);
        self.visit_expr(&*expr.then_branch);
        self.visit_expr(&*expr.else_branch);
        Default::default()
    }

    fn visit_return(&mut self, expr: &'ast Return, _node: &'ast Expr) -> ReturnType {
        self.visit_expr(&*expr.value);
        Default::default()
    }
}

pub trait MutVisitor<'ast, ReturnType = ()> : Sized 
    where ReturnType : Default {
    fn visit_module(&mut self, module: &'ast mut Module) -> ReturnType {
        for decl in &mut module.decls {
            self.visit_decl(decl);
        }
        Default::default()
    }

    fn visit_decl(&mut self, decl: &'ast mut Decl) -> ReturnType {
        match &mut decl.data {
            DeclKind::Function(func_decl) => self.visit_func_decl(func_decl),
            DeclKind::Variable(variable_decl) => self.visit_variable_decl(variable_decl),
        }
    }
    
    fn visit_func_decl(&mut self, func: &'ast mut FunctionDecl) -> ReturnType {
        self.visit_expr(&mut func.body);
        Default::default()
    }

    fn visit_variable_decl(&mut self, variable: &'ast mut VariableDecl) -> ReturnType {
        self.visit_expr(&mut variable.init);
        Default::default()
    }

    fn visit_expr(&mut self, node: &'ast mut Expr) -> ReturnType {
        match &mut node.data {
            ExprKind::Block(block_expr) => self.visit_block(block_expr),
            ExprKind::Decl(decl) => self.visit_decl(&mut **decl),
            ExprKind::IntegerLiteral(expr) => self.visit_integer_literal(expr),
            ExprKind::BooleanLiteral(expr) => self.visit_boolean_literal(expr),
            ExprKind::VariableReference(expr) => self.visit_variable_reference(expr),
            ExprKind::Binary(expr) => self.visit_binary_expr(expr),
            ExprKind::If(expr) => self.visit_if(expr),
            ExprKind::Return(expr) => self.visit_return(expr),
        }
    }

    fn visit_block(&mut self, block_expr: &'ast mut Block) -> ReturnType {
        for stmt in &mut block_expr.ignored_exprs {
            self.visit_expr(stmt);
        }
        if let Some(last_expr) = &mut block_expr.last_expr {
            self.visit_expr(&mut *last_expr);
        }
        Default::default()
    }

    fn visit_integer_literal(&mut self, _expr: &'ast mut IntegerLiteral) -> ReturnType {
        Default::default()
    }

    fn visit_boolean_literal(&mut self, _expr: &'ast mut BooleanLiteral) -> ReturnType {
        Default::default()
    }

    fn visit_variable_reference(&mut self, _expr: &'ast mut VariableReference) -> ReturnType {
        Default::default()
    }

    fn visit_binary_expr(&mut self, expr: &'ast mut BinaryExpr) -> ReturnType {
        self.visit_expr(&mut *expr.lhs);
        self.visit_expr(&mut *expr.rhs);
        Default::default()
    }

    fn visit_if(&mut self, expr: &'ast mut If) -> ReturnType {
        self.visit_expr(&mut *expr.condition);
        self.visit_expr(&mut *expr.then_branch);
        self.visit_expr(&mut *expr.else_branch);
        Default::default()
    }

    fn visit_return(&mut self, expr: &'ast mut Return) -> ReturnType {
        self.visit_expr(&mut *expr.value);
        Default::default()
    }
}
