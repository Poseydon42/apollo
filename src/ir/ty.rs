use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    Int, // FIXME: i32 for now, add more later
    Ptr,
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::Ptr => write!(f, "ptr"),
        }
    }
}
