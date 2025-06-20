use crate::ir;
use super::dag::*;
use super::isa;
use super::opcode::*;
use super::register_allocator::RegisterAllocationResult;
use std::fmt::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ISA;

impl ISA {
    pub fn new() -> Self {
        Self {}
    }
}

impl isa::ISA for ISA {
    type Opcode = Opcode;
    type Instruction = Instruction;
    type Register = Register;
    type Type = Type;

    fn name() -> &'static str {
        "x86_64"
    }

    fn lower_type(&self, ty: &ir::Ty) -> Self::Type {
        match ty {
            ir::Ty::Int => Type::DWord,
            ir::Ty::Ptr => Type::QWord,
        }
    }

    fn select_instruction(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let node = dag.get(instruction);
        assert!(node.is_generic(), "ISA shouldn't be asked to lower a native opcode");
        match node.opcode().get_generic() {
            GenericOpcode::Constant(..) => panic!("Constant nodes should not be a target for instruction selection"),

            GenericOpcode::Add |
            GenericOpcode::Sub => self.lower_simple_arithmetic(dag, instruction),

            GenericOpcode::Ret => self.lower_ret(dag, instruction),
            
            unsupported => panic!("Unsupported generic opcode {}", unsupported.to_string()),
        }
    }

    fn build_native_instruction(&self, dag: &DAG<Self>, instruction: &Node<Self>, register_allocation: &RegisterAllocationResult<Self>) -> Option<Self::Instruction> {
        if instruction.is_generic() {
            return None;
        }

        Some(match instruction.opcode().get_native() {
            Opcode::MOV => {
                let ty = instruction.get_output_type(0).get_native();
                let value = dag.get(instruction.get_input(0).node()).opcode().get_constant();
                let dst = register_allocation.value_map
                    .get(&instruction.get_output(0))
                    .expect("Destination register for MOVri should have been allocated");
                Instruction::two_args(
                    Opcode::MOV,
                    Operand::Register(*dst),
                    Operand::Immediate(value.bits_as_u64()),
                    *ty,
                )
            }

            Opcode::ADD |
            Opcode::SUB => {
                let ty = instruction.get_output_type(0).get_native();
                let lhs = register_allocation.value_map
                    .get(&instruction.get_input(0))
                    .expect("LHS register for ADD/SUB should have been allocated");
                let lhs = Operand::Register(*lhs);
                let rhs = if dag.get(instruction.get_input(1).node()).opcode().is_constant() {
                    let value = dag.get(instruction.get_input(1).node()).opcode().get_constant();
                    Operand::Immediate(value.bits_as_u64())
                } else {
                    let rhs = register_allocation.value_map
                        .get(&instruction.get_input(1))
                        .expect("RHS register for ADD/SUB should have been allocated");
                    Operand::Register(*rhs)
                };
                Instruction::two_args(
                    instruction.opcode().get_native(),
                    lhs,
                    rhs,
                    *ty,
                )
            }

            Opcode::RET => Instruction::no_args(instruction.opcode().get_native()),
        })
    }

    fn get_usable_registers(&self) -> Vec<Self::Register> {
        vec![
            Register::RAX,
            Register::RCX,
            Register::RDX,
            Register::R8,
            Register::R9,
            Register::R10,
            Register::R11,
        ]
    }
}

impl ISA {
    fn lower_simple_arithmetic(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        // NOTE: if the LHS is a constant, we need to move it into a register first
        // NOTE: this should technically never happen once we actually implement optimizations (constant folding and moving constants to the right)
        let lhs = if dag.get(dag.get(instruction).get_input(0).node()).opcode().is_constant() {
            let lhs_in_reg = dag.add_native_node(
                Opcode::MOV,
                vec![dag.get(instruction).get_input(0)],
                vec![OutputType::Native(dag.get(instruction).get_output_type(0).get_native().clone())],
            );
            dag.get_value(lhs_in_reg, 0)
        } else {
            dag.get(instruction).get_input(0)
        };
        let node = dag.get(instruction);
        let rhs = node.get_input(1);
        let ty = node.get_output_type(0).get_native().clone();

        if dag.get(lhs.node()).opcode().is_constant() {
        }

        let opcode = match node.opcode().get_generic() {
            GenericOpcode::Add => Opcode::ADD,
            GenericOpcode::Sub => Opcode::SUB,
            _ => unreachable!()
        };

        let node = dag.add_native_node(opcode, vec![lhs,rhs], vec![OutputType::Native(ty.clone())]);
        dag.replace_node(instruction, node);
    }

    fn lower_ret(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let ret = dag.get(instruction);
        let ctrl  = ret.get_input(0);
        let arg = ret.get_input(1);

        let ret = dag.add_native_node(Opcode::RET, vec![ctrl, arg], vec![]);
        dag.set_allowed_registers(ret, 1, vec![Register::RAX]);

        dag.replace_node(instruction, ret);
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Opcode {
    /// Notation:
    ///  - const: a constant value - the argument is provided by GenericOpcode::Constant
    ///  - reg:   a specific register - the argument is provided by GenericOpcode::Register
    ///  - vreg:  a virtual register - the argument is provided by any other generic or native node

    /// mov <vreg>, <const/reg/vreg>; (value) -> (value)
    MOV,

    /// add <vreg>, <const/reg/vreg>; (lhs, rhs) -> (value)
    ADD,

    /// sub <vreg>, <const/reg/vreg>; (lhs, rhs) -> (value)
    SUB,

    /// ret; (ctrl, value) -> ()4
    /// NOTE: the x86_64 ret instruction does not actually take any arguments, but we do take an argument here to ensure it is not eliminated by DCE
    RET,
}

impl isa::NativeOpcode for Opcode {
    fn is_input_overwritten_by_output(&self, input: PortId) -> Option<PortId> {
        match self {
            Opcode::ADD |
            Opcode::SUB => {
                match input {
                    0 => Some(0),
                    _ => None,
                }
            }

            Opcode::MOV |
            Opcode::RET => None,
        }   
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Opcode::MOV => write!(f, "MOV"),
            Opcode::ADD => write!(f, "ADD"),
            Opcode::SUB => write!(f, "SUB"),
            Opcode::RET => write!(f, "RET"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    Register(Register),
    Immediate(u64),
}

impl Operand {
    fn to_string(&self, ty: Type) -> String {
        match self {
            Operand::Register(reg) => reg.to_string(ty),
            Operand::Immediate(value) => ty.print_unsigned_in_hex(*value),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    opcode: Opcode,
    operands: [Option<(Operand,Type)>; 2],
}

impl Instruction {
    fn no_args(opcode: Opcode) -> Self {
        Self {
            opcode,
            operands: [None, None]
        }
    }

    fn two_args(opcode: Opcode, lhs: Operand, rhs: Operand, ty: Type) -> Self {
        Self {
            opcode,
            operands: [Some((lhs, ty)), Some((rhs, ty))]
        }
    }
}

impl isa::NativeInstruction for Instruction {}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.opcode)?;
        if let Some((lhs, ty)) = self.operands[0] {
            write!(f, " {}", lhs.to_string(ty))?;
        }
        if let Some((rhs, ty)) = self.operands[1] {
            write!(f, ", {}", rhs.to_string(ty))?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl isa::NativeRegister for Register {}

impl Register {
    fn to_string(&self, size: Type) -> String {
        match size {
            Type::Byte => match self {
                Register::RAX => "al".to_string(),
                Register::RBX => "bl".to_string(),
                Register::RCX => "cl".to_string(),
                Register::RDX => "dl".to_string(),
                Register::RSI => "sil".to_string(),
                Register::RDI => "dil".to_string(),
                Register::RSP => "spl".to_string(),
                Register::RBP => "bpl".to_string(),
                Register::R8 => "r8b".to_string(),
                Register::R9 => "r9b".to_string(),
                Register::R10 => "r10b".to_string(),
                Register::R11 => "r11b".to_string(),
                Register::R12 => "r12b".to_string(),
                Register::R13 => "r13b".to_string(),
                Register::R14 => "r14b".to_string(),
                Register::R15 => "r15b".to_string(),
            }
            Type::Word => match self {
                Register::RAX => "ax".to_string(),
                Register::RBX => "bx".to_string(),
                Register::RCX => "cx".to_string(),
                Register::RDX => "dx".to_string(),
                Register::RSI => "si".to_string(),
                Register::RDI => "di".to_string(),
                Register::RSP => "sp".to_string(),
                Register::RBP => "bp".to_string(),
                Register::R8 => "r8w".to_string(),
                Register::R9 => "r9w".to_string(),
                Register::R10 => "r10w".to_string(),
                Register::R11 => "r11w".to_string(),
                Register::R12 => "r12w".to_string(),
                Register::R13 => "r13w".to_string(),
                Register::R14 => "r14w".to_string(),
                Register::R15 => "r15w".to_string(),
            }
            Type::DWord => match self {
                Register::RAX => "eax".to_string(),
                Register::RBX => "ebx".to_string(),
                Register::RCX => "ecx".to_string(),
                Register::RDX => "edx".to_string(),
                Register::RSI => "esi".to_string(),
                Register::RDI => "edi".to_string(),
                Register::RSP => "esp".to_string(),
                Register::RBP => "ebp".to_string(),
                Register::R8 => "r8d".to_string(),
                Register::R9 => "r9d".to_string(),
                Register::R10 => "r10d".to_string(),
                Register::R11 => "r11d".to_string(),
                Register::R12 => "r12d".to_string(),
                Register::R13 => "r13d".to_string(),
                Register::R14 => "r14d".to_string(),
                Register::R15 => "r15d".to_string(),
            }
            Type::QWord => match self {
                Register::RAX => "rax".to_string(),
                Register::RBX => "rbx".to_string(),
                Register::RCX => "rcx".to_string(),
                Register::RDX => "rdx".to_string(),
                Register::RSI => "rsi".to_string(),
                Register::RDI => "rdi".to_string(),
                Register::RSP => "rsp".to_string(),
                Register::RBP => "rbp".to_string(),
                Register::R8 => "r8".to_string(),
                Register::R9 => "r9".to_string(),
                Register::R10 => "r10".to_string(),
                Register::R11 => "r11".to_string(),
                Register::R12 => "r12".to_string(),
                Register::R13 => "r13".to_string(),
                Register::R14 => "r14".to_string(),
                Register::R15 => "r15".to_string(),
            }
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.to_string(Type::QWord))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Byte,
    Word,
    DWord,
    QWord,
}

impl Type {
    fn print_unsigned_in_hex(&self, value: u64) -> String {
        match self {
            Type::Byte => format!("0x{:02x}", value),
            Type::Word => format!("0x{:04x}", value),
            Type::DWord => format!("0x{:08x}", value),
            Type::QWord => format!("0x{:016x}", value),
        }
    }
}

impl isa::NativeType for Type {}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Byte => "byte".to_string(),
            Type::Word => "word".to_string(),
            Type::DWord => "dword".to_string(),
            Type::QWord => "qword".to_string(),
        }
    }
}
