use super::isa::ISA;
use crate::ir;
use std::fmt::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenericOpcode<I: ISA> {
    // Special opcodes - they don't represent a real instruction

    /// Inputs:
    ///
    /// Outputs:
    ///   - the constant value
    Constant(ir::Constant),

    /// Inputs:
    /// 
    /// Outputs:
    ///  - the value in the register
    Register(I::Register),

    // Arithmetic opcodes
 
    /// Inputs:
    ///   - The first (left) operand
    ///   - The second (right) operand
    ///
    /// Outputs:
    ///   - Result
    Add, Sub,


    // Control flow

    /// Inputs:
    ///
    /// Outputs:
    ///   - control token
    Enter,

    /// Inputs:
    ///   - control token
    ///   - value to return
    ///
    /// Outputs:
    Ret,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Opcode<I: ISA> {
    Generic(GenericOpcode<I>),
    Native(I::Opcode),
}

impl<I: ISA> Opcode<I> {
    pub fn generic(opcode: GenericOpcode<I>) -> Self {
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

    pub fn get_generic(&self) -> &GenericOpcode<I> {
        match self {
            Opcode::Generic(opcode) => opcode,
            Opcode::Native(_) => panic!("Opcode is not generic"),
        }
    }

    pub fn get_native(&self) -> I::Opcode {
        match self {
            Opcode::Generic(_) => panic!("Opcode is not native"),
            Opcode::Native(opcode) => *opcode,
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Opcode::Generic(GenericOpcode::Constant(..)))
    }

    pub fn get_constant(&self) -> &ir::Constant {
        match self {
            Opcode::Generic(GenericOpcode::Constant(constant)) => constant,
            _ => panic!("Opcode is not a constant"),
        }
    }

    pub fn is_register(&self) -> bool {
        matches!(self, Opcode::Generic(GenericOpcode::Register(..)))
    }

    pub fn get_register(&self) -> &I::Register {
        match self {
            Opcode::Generic(GenericOpcode::Register(register)) => register,
            _ => panic!("Opcode is not a register"),
        }
    }
}

impl<I: ISA> Display for Opcode<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Opcode::Generic(opcode) => write!(f, "Generic{}", opcode.to_string()),
            Opcode::Native(opcode) => write!(f, "Native{}", opcode.to_string()),
        }
    }
}

impl<I: ISA> Display for GenericOpcode<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            GenericOpcode::Constant(constant) => write!(f, "Constant({})", constant),
            GenericOpcode::Register(register) => write!(f, "Register({})", register.to_string()),
            GenericOpcode::Add => write!(f, "Add"),
            GenericOpcode::Sub => write!(f, "Sub"),
            GenericOpcode::Enter => write!(f, "Enter"),
            GenericOpcode::Ret => write!(f, "Ret"),
        }
    }
}
