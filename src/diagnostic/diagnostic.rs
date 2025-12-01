use crate::span::Span;
use std::fmt::*;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Severity {
    Warning,
    Error,
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
        }
    }
}

pub trait Diagnostic {
    fn severity(&self) -> Severity;

    fn location(&self) -> &Span;

    fn message(&self) -> String;
}

#[macro_export]
macro_rules! declare_diagnostic {
    ($name:ident, $severity:ident, $format:literal, $($arg:ident: $ty:ty),*) => {
        #[derive(Clone)]
        pub(super) struct $name {
            $($arg : $ty,)*
            span: Span,
        }

        impl $name {
            pub fn new(span: Span, $($arg : $ty,)*) -> Self {
                Self {
                    span,
                    $($arg,)*
                }
            }
        }

        impl Diagnostic for $name {
            fn severity(&self) -> Severity {
                Severity::$severity
            }

            fn location(&self) -> &Span {
                &self.span
            }

            fn message(&self) -> String {
                format!($format, $(self.$arg,)*)
            }
        }
    };

    ($name:ident, $severity:ident, $format:literal) => { declare_diagnostic!($name, $severity, $format, ); }
}

#[macro_export]
macro_rules! create_diagnostic {
    ($name:ident, $location:expr, $($arg:expr),*) => {
        $name::new($location, $($arg,)*)
    };

    ($name:ident, $location:expr) => { create_diagnostic!($name, $location, ) }
}
