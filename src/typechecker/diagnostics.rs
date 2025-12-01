use crate::declare_diagnostic;
use crate::diagnostic::*;
use crate::span::Span;

declare_diagnostic!(MissingReturn, Error, "Function '{}' may not return a value on all paths; expected return type '{}'", function_name: String, return_type: String);
