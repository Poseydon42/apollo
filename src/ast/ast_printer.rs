use std::io::Write;
use super::*;

pub struct ASTPrinter<'writer, W: Write> {
    writer: &'writer mut W,
    indent_level: usize
}

impl<'writer, W: Write> ASTPrinter<'writer, W> {
    pub fn new(writer: &'writer mut W) -> Self {
        Self {
            writer,
            indent_level: 0
        }
    }

    fn get_type_string(&self, ty: &Type) -> String {
        match &ty.resolved {
            Some(resolved) => format!("{} => {}", ty.span.text(), resolved.to_string()),
            None => format!("{} => <unresolved>", ty.span.text()),
        }
    }
}

macro_rules! writeln_with_ident {
    ($self:expr, $fmt:literal $(,$arg:expr)*) => {
        write!($self.writer, "{}", "  ".repeat($self.indent_level)).unwrap();
        writeln!($self.writer, $fmt, $($arg,)*).unwrap()
    };
}

macro_rules! increase_indent_and {
    ($self:expr $(,$code:expr)+) => {
        $self.indent_level += 1;
        $($code;)+
        $self.indent_level -= 1;
    };
}

#[allow(unused_must_use)]
impl<'ast, 'writer, W: Write> Visitor<'ast> for ASTPrinter<'writer, W> {
    fn visit_func_decl(&mut self, func: &'ast FunctionDecl, node: &'ast Decl) {
        writeln_with_ident!(self, "FuncDecl @ ({},{}): {} -> {}", node.span.line(), node.span.column(), func.name.text(), self.get_type_string(&func.return_ty));
        increase_indent_and!(self, func.body.iter().for_each(|stmt| self.visit_stmt(stmt)));
    }

    fn visit_variable_decl(&mut self, variable: &'ast VariableDecl, node: &'ast Decl) {
        writeln_with_ident!(self, "VariableDecl @ ({},{}): {} : {}", node.span.line(), node.span.column(), variable.name.text(), self.get_type_string(&variable.ty));
        increase_indent_and!(self, self.visit_expr(&variable.init));
    }

    fn visit_integer_literal_expr(&mut self, expr: &'ast IntegerLiteralExpr, node: &'ast Expr) {
        writeln_with_ident!(self, "IntegerLiteralExpr @ ({},{}): {}", node.span.line(), node.span.column(), expr.raw_value.text());
    }

    fn visit_variable_reference_expr(&mut self, expr: &'ast VariableReferenceExpr, node: &'ast Expr) {
        writeln_with_ident!(self, "VariableReferenceExpr @ ({},{}): {}", node.span.line(), node.span.column(), expr.name.text());
    }

    fn visit_binary_expr(&mut self, expr: &'ast BinaryExpr, node: &'ast Expr) -> () {
        writeln_with_ident!(self, "BinaryExpr @ ({},{}): {}", node.span.line(), node.span.column(), expr.op);
        increase_indent_and!(self, self.visit_expr(&*expr.lhs), self.visit_expr(&*expr.rhs));
    }

    fn visit_return_stmt(&mut self, value: &'ast Expr, stmt: &'ast Stmt) {
        writeln_with_ident!(self, "ReturnStmt @ ({},{})", stmt.span.line(), stmt.span.column());
        increase_indent_and!(self, self.visit_expr(value));
    }
}
