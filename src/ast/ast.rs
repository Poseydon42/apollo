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
pub struct FunctionArg {
    pub name: Span,
    pub ty: Type,
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Span,
    pub args: Vec<FunctionArg>,
    pub return_ty: Type,
    pub body: Expr,
}

#[derive(Debug)]
pub struct VariableDecl {
    pub name: Span,
    pub ty: Type,
    pub init: Expr,
}

#[derive(Debug)]
pub struct Block {
    pub ignored_exprs: Vec<Expr>,
    pub last_expr: Option<Box<Expr>>,
}

impl Block {
    pub fn new(ignored_exprs: Vec<Expr>, last_expr: Option<Expr>) -> Self {
        Self {
            ignored_exprs,
            last_expr: last_expr.map(Box::new),
        }
    }
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
pub struct IntegerLiteral {
    pub raw_value: Span,
    pub ty: Option<ResolvedType>,
}

impl IntegerLiteral {
    pub fn new(raw_value: Span) -> Self {
        Self {
            raw_value,
            ty: None,
        }
    }
}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub value: bool,
}

impl BooleanLiteral {
    pub fn new(value: bool) -> Self {
        Self {
            value,
        }
    }
}

#[derive(Debug)]
pub struct VariableReference {
    pub name: Span,
    pub ty: Option<ResolvedType>,
}

impl VariableReference {
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
pub struct If {
    pub condition: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Box<Expr>,
}

impl If {
    pub fn new(condition: Expr, then_branch: Expr, else_branch: Expr) -> Self {
        Self {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }
}

#[derive(Debug)]
pub struct Return {
    pub value: Box<Expr>,
}

impl Return {
    pub fn new(value: Expr) -> Self {
        Self {
            value: Box::new(value),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Block(Block),

    Decl(Box<Decl>),

    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    VariableReference(VariableReference),

    Binary(BinaryExpr),

    If(If),

    Return(Return),
}

pub type Expr = Node<ExprKind>;

