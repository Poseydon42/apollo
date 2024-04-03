use std::ops::Deref;
use crate::ast::*;

pub trait Visitor<'ast, ReturnType = ()> : Sized where
    ReturnType : Default {
    fn visit_module(&mut self, module: &'ast Module) -> ReturnType {
        walk_module(module, self);
        Default::default()
    }

    fn visit_decl(&mut self, decl: &'ast Decl) -> ReturnType {
        match &decl.data {
            DeclKind::Function(func_decl) => self.visit_func_decl(&func_decl, decl),
        }
    }

    fn visit_func_decl(&mut self, func: &'ast FunctionDecl, _node: &'ast Decl) -> ReturnType {
        walk_func_decl(func, self);
        Default::default()
    }

    fn visit_name_ty(&mut self, _node: &'ast Ty) -> ReturnType {
        Default::default()
    }

    fn visit_expr(&mut self, expr: &'ast Expr) -> ReturnType {
        match &expr.data {
            ExprKind::IntegerLiteral => self.visit_integer_literal_expr(expr),
            ExprKind::Binary { lhs, op, rhs } => self.visit_binary_expr(lhs.deref(), *op, rhs.deref(), expr)
        }
    }

    fn visit_integer_literal_expr(&mut self, _node: &'ast Expr) -> ReturnType {
        Default::default()
    }

    fn visit_binary_expr(&mut self, lhs: &'ast Expr, _op: BinaryOp, rhs: &'ast Expr, _expr: &'ast Expr) -> ReturnType {
        self.visit_expr(lhs);
        self.visit_expr(rhs);
        Default::default()
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> ReturnType {
        match &stmt.data {
            StmtKind::Return(value) => self.visit_return_stmt(value, stmt)
        }
    }

    fn visit_return_stmt(&mut self, value: &'ast Expr, _stmt: &'ast Stmt) -> ReturnType {
        self.visit_expr(value);
        Default::default()
    }
}

pub fn walk_module<'ast, T: Default, V: Visitor<'ast, T>>(module: &'ast Module, visitor: &mut V) {
    for decl in &module.decls {
        visitor.visit_decl(decl);
    }
}

pub fn walk_func_decl<'ast, T: Default, V: Visitor<'ast, T>>(func: &'ast FunctionDecl, visitor: &mut V) {
    for stmt in &func.body {
        visitor.visit_stmt(stmt);
    }
}
