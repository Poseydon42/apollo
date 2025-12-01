use std::fmt::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedType {
    Empty,
    BuiltIn(BuiltInType),
}

impl ResolvedType {
    pub fn to_string(&self) -> String {
        match self {
            ResolvedType::Empty => "()".to_string(),
            ResolvedType::BuiltIn(ty) => ty.to_string(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BuiltInType {
    Int, // FIXME: this is i32 for now, add more
}

impl Display for BuiltInType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            BuiltInType::Int => write!(f, "i32"),
        }
    }
}
