use crate::declare_diagnostic;
use crate::diagnostic::*;
use crate::lexer::LexemKind;
use crate::span::Span;

declare_diagnostic!(UnexpectedEOF, Error, "Reached end of file while expecting lexem {}", expected: LexemKind);
declare_diagnostic!(UnexpectedLexem, Error, "Expected lexem {}, got {}", expected: LexemKind, actual: LexemKind);
