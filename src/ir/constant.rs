use super::Ty;
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constant {
    ty: Ty,
    bytes: Vec<u8>,
}

impl Constant {
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

    pub fn bits_as_u32(&self) -> u32 {
        assert_eq!(self.ty, Ty::Int);
        assert_eq!(self.bytes.len(), 4);
        u32::from_le_bytes(self.bytes[0..4].try_into().unwrap())
    }

    pub fn bits_as_u64(&self) -> u64 {
        // FIXME: so so bad
        self.bits_as_u32() as u64
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            Ty::Int => write!(f, "{} {}", self.ty, self.bits_as_u32()),
            Ty::Ptr => panic!("Pointer constants are not supported"),
        }
    }
}