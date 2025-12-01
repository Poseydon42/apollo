use super::Ty;
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constant {
    ty: Ty,
    bytes: Vec<u8>,
}

impl Constant {
    pub fn bool(value: bool) -> Self {
        Self {
            ty: Ty::Bool,
            bytes: vec![if value { 1 } else { 0 }],
        }
    }

    pub fn int(value: i32) -> Self {
        Self {
            ty: Ty::Int,
            bytes: value.to_le_bytes().to_vec(),
        }
    }

    pub fn ty(&self) -> &Ty {
        &self.ty
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn bits_as_u64(&self) -> u64 {
        let mut value: u64 = 0;
        for (i, byte) in self.bytes.iter().enumerate() {
            value |= (*byte as u64) << (i * 8);
        }
        value
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            Ty::Bool => write!(f, "{}", self.bytes.get(0).expect("Bool constant must have 1 byte") != &0),
            Ty::Int => write!(f, "{} {}", self.ty, self.bits_as_u64() & 0xFFFF_FFFF),
            Ty::Ptr => panic!("Pointer constants are not supported"),
        }
    }
}