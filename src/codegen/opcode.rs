use super::isa::ISA;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GenericOpcode {
    // Special opcodes - they don't represent a real instruction

    /// Inputs:
    /// Outputs:
    ///   - the constant value
    Constant,

    // Control flow

    /// Inputs:
    /// Outputs:
    ///   - control token
    Enter,

    /// Inputs:
    ///   - control token
    ///   - value to return
    /// Outputs:
    Ret,
}

pub enum Opcode<I: ISA> {
    Generic(GenericOpcode),
    Native(I::Opcode),
}

impl<I: ISA> Copy for Opcode<I> {}
impl<I: ISA> Clone for Opcode<I> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<I: ISA> Opcode<I> {
    pub fn generic(opcode: GenericOpcode) -> Self {
        Opcode::Generic(opcode)
    }

    pub fn native(opcode: I::Opcode) -> Self {
        Opcode::Native(opcode)
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Opcode::Generic(_) => true,
            Opcode::Native(_) => false,
        }
    }

    pub fn is_native(&self) -> bool {
        !self.is_generic()
    }

    pub fn get_generic(&self) -> GenericOpcode {
        match self {
            Opcode::Generic(opcode) => *opcode,
            Opcode::Native(_) => panic!("Opcode is not generic"),
        }
    }

    pub fn get_native(&self) -> I::Opcode {
        match self {
            Opcode::Generic(_) => panic!("Opcode is not native"),
            Opcode::Native(opcode) => *opcode,
        }
    }
}

impl<I: ISA> ToString for Opcode<I> {
    fn to_string(&self) -> String {
        match self {
            Opcode::Generic(opcode) => format!("Generic{}", opcode.to_string()),
            Opcode::Native(opcode) => format!("Native{}", opcode.to_string()),
        }
    }
}

impl ToString for GenericOpcode {
    fn to_string(&self) -> String {
        match self {
            GenericOpcode::Constant => "Constant".to_string(),
            GenericOpcode::Enter => "Enter".to_string(),
            GenericOpcode::Ret => "Ret".to_string(),
        }
    }
}