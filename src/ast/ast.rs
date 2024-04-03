use crate::span::Span;
use super::{Node, Ty};
use std::fmt::*;

#[derive(Debug)]
pub struct Module {
    pub decls: Vec<Decl>,
}

impl Module {
    pub fn new(decls: Vec<Decl>) -> Self {
        Self {
            decls
        }
    }
}

#[derive(Debug)]
pub enum DeclKind {
    Function(FunctionDecl),
}

pub type Decl = Node<DeclKind>;

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Span,
    pub return_ty: Ty,
    pub body: Vec<Stmt>,
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    IntegerLiteral,
    Binary { lhs: Box<Expr>, op: BinaryOp, rhs: Box<Expr> },
}

pub type Expr = Node<ExprKind>;

#[derive(Debug)]
pub enum StmtKind {
    Return(Expr),
}

pub type Stmt = Node<StmtKind>;
