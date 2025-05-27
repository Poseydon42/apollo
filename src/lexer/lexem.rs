use crate::span::Span;
use std::fmt::*;
use std::rc::Rc;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum LexemKind {
    Identifier,

    IntegerLiteral,

    Fn,
    Let,
    Return,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Arrow,
    Colon,
    Semicolon,
    Plus,
    Minus,
    Equals,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Lexem {
    pub kind: LexemKind,
    pub span: Span,
}

impl Lexem {
    pub fn new(kind: LexemKind, src: Rc<String>, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: Span::new(src, start, end)
        }
    }

    pub fn text(&self) -> &str {
        self.span.text()
    }
}

impl Display for Lexem {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use LexemKind::*;
        match &self.kind {
            Identifier | IntegerLiteral => write!(f, "{}({}) @({},{})", self.kind, self.span.text(), self.span.line(), self.span.column()),
            _ => write!(f, "{} @({}, {})", self.kind, self.span.line(), self.span.column())
        }
    }
}

pub(super) fn is_single_char_lexem(c: char) -> Option<LexemKind> {
    match c {
        '(' => Some(LexemKind::LeftParen),
        ')' => Some(LexemKind::RightParen),
        '{' => Some(LexemKind::LeftBrace),
        '}' => Some(LexemKind::RightBrace),
        ':' => Some(LexemKind::Colon),
        ';' => Some(LexemKind::Semicolon),
        '+' => Some(LexemKind::Plus),
        '-' => Some(LexemKind::Minus),
        '=' => Some(LexemKind::Equals),
        _ => None,
    }
}

pub(super) fn is_double_char_lexem(first: char, second: char) -> Option<LexemKind> {
    match (first, second) {
        ('-', '>') => Some(LexemKind::Arrow),
        _ => None,
    }
}

pub(super) fn is_keyword(identifier: &str) -> Option<LexemKind> {
    match identifier {
        "fn" => Some(LexemKind::Fn),
        "let" => Some(LexemKind::Let),
        "return" => Some(LexemKind::Return),
        _ => None,
    }
}

impl Display for LexemKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            LexemKind::Identifier => write!(f, "Identifier"),
            LexemKind::IntegerLiteral => write!(f, "IntegerLiteral"),
            LexemKind::Fn => write!(f, "Fn"),
            LexemKind::Let => write!(f, "Let"),
            LexemKind::Return => write!(f, "Return"),
            LexemKind::LeftParen => write!(f, "LeftParen"),
            LexemKind::RightParen => write!(f, "RightParen"),
            LexemKind::LeftBrace => write!(f, "LeftBrace"),
            LexemKind::RightBrace => write!(f, "RightBrace"),
            LexemKind::Arrow => write!(f, "Arrow"),
            LexemKind::Colon => write!(f, "Colon"),
            LexemKind::Semicolon => write!(f, "Semicolon"),
            LexemKind::Plus => write!(f, "Plus"),
            LexemKind::Minus => write!(f, "Minus"),
            LexemKind::Equals => write!(f, "Equals"),
        }
    }
}