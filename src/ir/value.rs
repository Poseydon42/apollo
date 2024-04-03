use super::Ty;

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub ty: Ty,
    pub kind: ValueKind,
    pub data: ValueData,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValueKind {
    Immediate,
}

impl Value {
    pub fn immediate_int(value: i32) -> Self {
        Self {
            ty: Ty::Int,
            kind: ValueKind::Immediate,
            data: ValueData::Int(value),
        }
    }

    pub fn bits_as_u64(&self) -> u64 {
        match self.data {
            ValueData::Int(value) => value as u64
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueData {
    Int(i32), // FIXME: this will need to be extended
}

impl ValueData {
    pub fn as_int(&self) -> i32 {
        match self {
            ValueData::Int(value) => *value
        }
    }
}