use crate::declare_diagnostic;
use crate::diagnostic::*;
use crate::span::Span;

declare_diagnostic!(IfConditionNotBool, Error, "Condition of 'if' expression must be of type 'bool', found '{}'", found_type: String);
declare_diagnostic!(IfBranchesTypeMismatch, Error, "Branches of 'if' expression must evaluate to the same type, found '{}' and '{}'", then_type: String, else_type: String);

declare_diagnostic!(MissingReturn, Error, "Function '{}' may not return a value on all paths; expected return type '{}'", function_name: String, return_type: String);
