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
        writeln_with_ident!(self, "FuncDecl @ ({},{}): {} -> {}", node.span.line(), node.span.column(), func.name.text(), func.return_ty.to_string());
        increase_indent_and!(self, walk_func_decl(func, self));
    }

    fn visit_integer_literal_expr(&mut self, node: &'ast Expr) {
        writeln_with_ident!(self, "IntegerLiteralExpr @ ({},{}): {}", node.span.line(), node.span.column(), node.span.text());
    }

    fn visit_binary_expr(&mut self, lhs: &'ast Expr, op: BinaryOp, rhs: &'ast Expr, _expr: &'ast Expr) -> () {
        writeln_with_ident!(self, "BinaryExpr @ ({},{}): {}", lhs.span.line(), lhs.span.column(), op);
        increase_indent_and!(self, self.visit_expr(lhs), self.visit_expr(rhs));
    }

    fn visit_return_stmt(&mut self, value: &'ast Expr, stmt: &'ast Stmt) {
        writeln_with_ident!(self, "ReturnStmt @ ({},{})", stmt.span.line(), stmt.span.column());
        increase_indent_and!(self, self.visit_expr(value));
    }
}