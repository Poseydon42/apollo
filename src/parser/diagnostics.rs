use crate::declare_diagnostic;
use crate::diagnostic::*;
use crate::lexer::LexemKind;
use crate::span::Span;

declare_diagnostic!(UnexpectedEOF, Error, "Reached end of file while expecting lexem {}", expected: LexemKind);
declare_diagnostic!(UnexpectedLexem, Error, "Expected lexem {}, got {}", expected: LexemKind, actual: LexemKind);

declare_diagnostic!(UnexpectedExprAfterLastExprInBlock, Error, "Unexpected expression after last expression in block (or the previous expression is missing a semicolon)");
