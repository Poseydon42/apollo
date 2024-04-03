#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    BuiltIn(BuiltInTy),
}

impl Ty {
    pub fn to_string(&self) -> String {
        match self {
            Ty::BuiltIn(ty) => ty.to_string(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BuiltInTy {
    Int, // FIXME: this is i32 for now, add more
}

impl ToString for BuiltInTy {
    fn to_string(&self) -> String {
        match self {
            BuiltInTy::Int => "int".to_string(),
        }
    }
}
