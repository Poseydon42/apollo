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
        for stmt in &func.body {
            self.visit_stmt(stmt);
        }
        Default::default()
    }

    fn visit_variable_decl(&mut self, variable: &'ast VariableDecl, _node: &'ast Decl) -> ReturnType {
        self.visit_expr(&variable.init);
        Default::default()
    }

    fn visit_expr(&mut self, expr: &'ast Expr) -> ReturnType {
        match &expr.data {
            ExprKind::IntegerLiteral(literal_expr) => self.visit_integer_literal_expr(literal_expr, expr),
            ExprKind::VariableReference(variable_reference_expr) => self.visit_variable_reference_expr(variable_reference_expr, expr),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(binary_expr, expr)
        }
    }

    fn visit_integer_literal_expr(&mut self, _expr: &'ast IntegerLiteralExpr, _node: &'ast Expr) -> ReturnType {
        Default::default()
    }

    fn visit_variable_reference_expr(&mut self, _expr: &'ast VariableReferenceExpr, _node: &'ast Expr) -> ReturnType {
        Default::default()
    }

    fn visit_binary_expr(&mut self, expr: &'ast BinaryExpr, _node: &'ast Expr) -> ReturnType {
        self.visit_expr(&*expr.lhs);
        self.visit_expr(&*expr.rhs);
        Default::default()
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> ReturnType {
        match &stmt.data {
            StmtKind::Decl(decl) => self.visit_decl_stmt(decl),
            StmtKind::Return(value) => self.visit_return_stmt(value, stmt)
        }
    }

    fn visit_decl_stmt(&mut self, decl: &'ast Decl) -> ReturnType {
        self.visit_decl(decl);
        Default::default()
    }

    fn visit_return_stmt(&mut self, value: &'ast Expr, _stmt: &'ast Stmt) -> ReturnType {
        self.visit_expr(value);
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
        for stmt in &mut func.body {
            self.visit_stmt(stmt);
        }
        Default::default()
    }

    fn visit_variable_decl(&mut self, variable: &'ast mut VariableDecl) -> ReturnType {
        self.visit_expr(&mut variable.init);
        Default::default()
    }

    fn visit_expr(&mut self, node: &'ast mut Expr) -> ReturnType {
        match &mut node.data {
            ExprKind::IntegerLiteral(expr) => self.visit_integer_literal_expr(expr),
            ExprKind::VariableReference(expr) => self.visit_variable_reference_expr(expr),
            ExprKind::Binary(expr) => self.visit_binary_expr(expr),
        }
    }

    fn visit_integer_literal_expr(&mut self, _expr: &'ast mut IntegerLiteralExpr) -> ReturnType {
        Default::default()
    }

    fn visit_variable_reference_expr(&mut self, _expr: &'ast mut VariableReferenceExpr) -> ReturnType {
        Default::default()
    }

    fn visit_binary_expr(&mut self, expr: &'ast mut BinaryExpr) -> ReturnType {
        self.visit_expr(&mut *expr.lhs);
        self.visit_expr(&mut *expr.rhs);
        Default::default()
    }

    fn visit_stmt(&mut self, stmt: &'ast mut Stmt) -> ReturnType {
        match &mut stmt.data {
            StmtKind::Decl(decl) => self.visit_decl_stmt(decl),
            StmtKind::Return(value) => self.visit_return_stmt(value)
        }
    }

    fn visit_decl_stmt(&mut self, decl: &'ast mut Decl) -> ReturnType {
        self.visit_decl(decl);
        Default::default()
    }

    fn visit_return_stmt(&mut self, value: &'ast mut Expr) -> ReturnType {
        self.visit_expr(value);
        Default::default()
    }
}
