use crate::span::Span;
use super::{Node, ResolvedType};
use std::fmt::*;

#[derive(Debug)]
pub struct Type {
    pub span: Span,
    pub resolved: Option<ResolvedType>,
}

impl Type {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            resolved: None,
        }
    }
}

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
    Variable(VariableDecl),
}

pub type Decl = Node<DeclKind>;

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Span,
    pub return_ty: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct VariableDecl {
    pub name: Span,
    pub ty: Type,
    pub init: Expr,
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
pub struct IntegerLiteralExpr {
    pub raw_value: Span,
    pub ty: Option<ResolvedType>,
}

impl IntegerLiteralExpr {
    pub fn new(raw_value: Span) -> Self {
        Self {
            raw_value,
            ty: None,
        }
    }
}

#[derive(Debug)]
pub struct VariableReferenceExpr {
    pub name: Span,
    pub ty: Option<ResolvedType>,
}

impl VariableReferenceExpr {
    pub fn new(name: Span) -> Self {
        Self {
            name,
            ty: None,
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
}

impl BinaryExpr {
    pub fn new(lhs: Expr, op: BinaryOp, rhs: Expr) -> Self {
        Self {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    IntegerLiteral(IntegerLiteralExpr),
    VariableReference(VariableReferenceExpr),
    Binary(BinaryExpr),
}

pub type Expr = Node<ExprKind>;

#[derive(Debug)]
pub enum StmtKind {
    Decl(Decl),
    Return(Expr),
}

pub type Stmt = Node<StmtKind>;
