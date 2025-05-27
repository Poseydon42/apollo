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
        }
    }

    fn select_instruction(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let node = dag.get(instruction);
        assert!(node.is_generic(), "ISA shouldn't be asked to lower a native opcode");
        match node.opcode().get_generic() {
            GenericOpcode::Constant => self.lower_constant(dag, instruction),

            GenericOpcode::Add |
            GenericOpcode::Sub => self.lower_simple_arithmetic(dag, instruction),

            GenericOpcode::Ret => self.lower_ret(dag, instruction),
            
            unsupported => panic!("Unsupported generic opcode {}", unsupported.to_string()),
        }
    }

    fn build_native_instruction(&self, instruction: &Node<Self>, register_allocation: &RegisterAllocationResult<Self>) -> Option<Self::Instruction> {
        if instruction.is_generic() {
            return None;
        }

        Some(match instruction.opcode().get_native() {
            Opcode::MOVri => {
                let ty = instruction.get_output_type(0).get_native();
                let value = instruction.payload().get_constant().clone();
                let dst = register_allocation.value_map
                    .get(&instruction.get_output(0))
                    .expect("Destination register for MOVri should have been allocated");
                Instruction::two_args(
                    Opcode::MOVri,
                    Operand::Register(*dst),
                    Operand::Immediate(value.bits_as_u64()),
                    *ty,
                )
            }

            Opcode::ADDrr |
            Opcode::SUBrr => {
                let lhs = register_allocation.value_map
                    .get(&instruction.get_input(0))
                    .expect("Left-hand side register for ADDrr/SUBrr should have been allocated");
                let rhs = register_allocation.value_map
                    .get(&instruction.get_input(1))
                    .expect("Right-hand side register for ADDrr/SUBrr should have been allocated");
                let ty = instruction.get_output_type(0).get_native();
                Instruction::two_args(
                    instruction.opcode().get_native(),
                    Operand::Register(*lhs),
                    Operand::Register(*rhs),
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
    fn lower_constant(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let value = dag.get(instruction).payload().get_constant().clone();
        let node = self.mov_reg_imm(dag, value);
        dag.replace_node(instruction, node.node());
    }

    fn lower_simple_arithmetic(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let node = dag.get(instruction);
        let lhs = node.get_input(0);
        let rhs = node.get_input(1);
        let ty = node.get_output_type(0).get_native();

        let opcode = match node.opcode().get_generic() {
            GenericOpcode::Add => Opcode::ADDrr,
            GenericOpcode::Sub => Opcode::SUBrr,
            _ => panic!("Unsupported generic opcode for arithmetic operation"),
        };

        let node = dag.add_native_node(
            opcode, 
            vec![
                lhs,
                rhs],
            vec![OutputType::Native(ty.clone())]
        );
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

    // mov reg, arg
    fn mov_reg_imm(&self, dag: &mut DAG<Self>, value: ir::Constant) -> Value {
        let ty = isa::ISA::lower_type(self, value.ty());
        let mov = dag.add_native_node_with_payload(
            Opcode::MOVri,
            NodePayload::Constant(value),
            vec![],
            vec![OutputType::Native(ty)],
        );
        dag.get_value(mov, 0)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Opcode {
    /// mov <reg>, <payload constant>; () -> (value)
    MOVri,

    /// add <reg>, <reg>; (lhs, rhs) -> (value)
    ADDrr,

    /// add <reg>, <reg>; (lhs, rhs) -> (value)
    SUBrr,

    /// ret; (ctrl, value) -> ()
    RET,
}

impl isa::NativeOpcode for Opcode {
    fn is_input_overwritten_by_output(&self, input: PortId) -> Option<PortId> {
        match self {
            Opcode::ADDrr |
            Opcode::SUBrr => {
                match input {
                    0 => Some(0),
                    _ => None,
                }
            }

            Opcode::MOVri |
            Opcode::RET => None,
        }   
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Opcode::MOVri => write!(f, "MOV"),
            Opcode::ADDrr => write!(f, "ADD"),
            Opcode::SUBrr => write!(f, "SUB"),
            Opcode::RET => write!(f, "RET"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    Register(Register),
    Immediate(u64),
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

impl ToString for Instruction {
    fn to_string(&self) -> String {
        match self.opcode {
            Opcode::MOVri => match (self.operands[0], self.operands[1]) {
                (Some((dst, dst_ty)), Some((src, src_ty))) => {
                    assert!(dst_ty == src_ty, "Operands of mov must have the same type");
                    match (dst, src) {
                        (Operand::Register(reg), Operand::Immediate(imm)) =>
                            format!("mov {}, {}", reg.to_string(), src_ty.print_unsigned_in_hex(imm)),

                        _ => panic!("Invalid combination of operands for mov")
                    }
                }

                _ => panic!("MOV instruction must have two operands")
            }

            Opcode::ADDrr |
            Opcode::SUBrr => match (self.operands[0], self.operands[1]) {
                (Some((lhs, lhs_ty)), Some((rhs, rhs_ty))) => {
                    assert!(lhs_ty == rhs_ty, "Operands of arithmetic operations must have the same type");
                    match (lhs, rhs) {
                        (Operand::Register(lhs_reg), Operand::Register(rhs_reg)) =>
                            format!("{} {}, {}", self.opcode.to_string().to_lowercase(), lhs_reg.to_string(), rhs_reg.to_string()),

                        _ => panic!("Invalid combination of operands for {}", self.opcode)
                    }
                }

                _ => panic!("Arithmetic instruction must have two operands")
            }

            Opcode::RET => "ret".to_string()
        }
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

impl ToString for Register {
    fn to_string(&self) -> String {
        match self {
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
