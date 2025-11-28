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

        #[allow(unreachable_patterns)]
        match node.opcode().get_generic() {
            GenericOpcode::Enter        |
            GenericOpcode::Constant(..) |
            GenericOpcode::Register(..) => panic!("Special generic node {} should not be a target for instruction selection", node.opcode().get_generic()),

            GenericOpcode::Add |
            GenericOpcode::Sub => self.lower_simple_arithmetic(dag, instruction),

            GenericOpcode::Load => self.lower_load(dag, instruction),
            GenericOpcode::Store => self.lower_store(dag, instruction),

            GenericOpcode::Ret => self.lower_ret(dag, instruction),

            _ => todo!()
        }
    }

    fn insert_register_copy(&self, dag: &mut DAG<Self>, value: Value) -> Value {
        let ty = dag.get_value_type(value).clone();
        let copy_node = dag.add_native_node(
            Opcode::MOV,
            vec![value],
            vec![ty],
        );
        dag.get_value(copy_node, 0)
    }

    fn build_native_instruction(&self, dag: &DAG<Self>, instruction: &Node<Self>, register_allocation: &RegisterAllocationResult<Self>) -> Option<Self::Instruction> {
        if instruction.is_generic() {
            return None;
        }

        Some(match instruction.opcode().get_native() {
            Opcode::MOV => {
                let ty = instruction.get_output_type(0).get_native();
                let value_opcode = dag.get(instruction.get_input(0).node()).opcode();
                let value = if value_opcode.is_constant() {
                    Operand::Immediate(value_opcode.get_constant().bits_as_u64())
                } else if value_opcode.is_register() {
                    Operand::Register(*value_opcode.get_register())
                } else {
                    unimplemented!();
                };
                let dst = register_allocation.value_map
                    .get(&instruction.get_output(0))
                    .expect("Destination register for MOVri should have been allocated");
                Instruction::two_args(
                    Opcode::MOV,
                    Operand::Register(*dst),
                    value,
                    *ty,
                )
            }

            Opcode::MOV_LOAD => {
                let output = register_allocation.value_map
                    .get(&instruction.get_output(1))
                    .expect("Output register for MOV_LOAD should have been allocated");
                let ty = instruction.get_output_type(1).get_native();

                let addr = self.create_operand_for_address(dag, instruction.inputs().skip(1).cloned().collect(), register_allocation);

                Instruction::two_args(
                    Opcode::MOV_LOAD,
                    Operand::Register(*output),
                    addr,
                    *ty,
                )
            }

            Opcode::MOV_STORE => {
                // First operand is control token, last operand is value to store
                let address_operands = instruction.inputs()
                    .skip(1)
                    .take(instruction.inputs().count() - 2)
                    .cloned()
                    .collect();
                let addr = self.create_operand_for_address(dag, address_operands, register_allocation);

                let value_node = dag.get(instruction.get_input(2).node());
                let value = if value_node.opcode().is_constant() {
                    Operand::Immediate(value_node.opcode().get_constant().bits_as_u64())
                } else {
                    Operand::Register(
                        *register_allocation.value_map
                        .get(&instruction.get_input(2))
                        .expect("Non-immediate value of MOV_STORE should have been allocated a register")
                    )
                };
                Instruction::two_args(
                    Opcode::MOV_STORE,
                    addr,
                    value,
                    dag.get_value_type(instruction.get_input(2)).get_native().clone()
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

            _ => unimplemented!(),
        })
    }

    fn generate_prologue(&self) -> Vec<Self::Instruction> {
        vec![
            Instruction::one_arg(Opcode::PUSH, Operand::Register(Register::RBP), self.get_pointer_type()),
            Instruction::two_args(Opcode::MOV, Operand::Register(Register::RBP), Operand::Register(Register::RSP), self.get_pointer_type()),
        ]
    }

    fn generate_epilogue(&self) -> Vec<Self::Instruction> {
        vec![
            Instruction::two_args(Opcode::MOV, Operand::Register(Register::RSP), Operand::Register(Register::RBP), self.get_pointer_type()),
            Instruction::one_arg(Opcode::POP, Operand::Register(Register::RBP), self.get_pointer_type()),
        ]
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

    fn get_stack_frame_base_register(&self) -> Self::Register {
        Register::RBP
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

    fn lower_load(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let load = dag.get(instruction);
        let ctrl = load.get_input(0);
        let location = load.get_input(1);
        let ty = load.get_output_type(1).get_native();

        let inputs = match self.match_memory_address(dag, location) {
            MemoryAddressMatch::Direct { value } => vec![ ctrl, value ],
            MemoryAddressMatch::BasePlusOffset { base, offset } => vec![ ctrl, base, offset ],
        };

        let load = dag.add_native_node(
            Opcode::MOV_LOAD,
            inputs,
            vec![
                OutputType::Control,
                OutputType::Native(ty.clone())
            ]
        );
        dag.replace_node(instruction, load);
    }

    fn lower_store(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let store = dag.get(instruction);
        let ctrl = store.get_input(0);
        let location = store.get_input(1);
        let value = store.get_input(2);

        let inputs = match self.match_memory_address(dag, location) {
            MemoryAddressMatch::Direct { value: address } => vec![ ctrl, address, value ],
            MemoryAddressMatch::BasePlusOffset { base, offset } => vec![ ctrl, base, offset, value ],
        };

        let store = dag.add_native_node(
            Opcode::MOV_STORE,
            inputs,
            vec![
                OutputType::Control
            ]
        );
        dag.replace_node(instruction, store);
    }

    fn lower_ret(&self, dag: &mut DAG<Self>, instruction: NodeId) {
        let ret = dag.get(instruction);
        let ctrl  = ret.get_input(0);
        let arg = ret.get_input(1);

        let ret = dag.add_native_node(Opcode::RET, vec![ctrl, arg], vec![]);
        dag.set_allowed_registers(ret, 1, vec![Register::RAX]);

        dag.replace_node(instruction, ret);
    }

    fn match_memory_address(&self, dag: &DAG<Self>, address: Value) -> MemoryAddressMatch {
        let address_node = dag.get(address.node());
        if !address_node.opcode().is_generic() {
            return MemoryAddressMatch::Direct { value: address };
        }

        let opcode = address_node.opcode().get_generic();
        match opcode {
            GenericOpcode::Add => {
                let base = address_node.get_input(0);
                let offset = address_node.get_input(1);
                let offset_opcode = dag.get(offset.node()).opcode();
                if !offset_opcode.is_constant() {
                    // TODO: offsets can be registers too, the thing we're missing towards handling this
                    //       is encoding memory operands with [reg + reg] format. This will become necessary
                    //       when we implement array access.
                    return MemoryAddressMatch::Direct { value: address }
                }
                MemoryAddressMatch::BasePlusOffset {
                    base,
                    offset,
                }
            }

            _ => MemoryAddressMatch::Direct { value: address },
        }
    }

    fn create_operand_for_address(&self, dag: &DAG<Self>, address_inputs: Vec<Value>, register_allocation: &RegisterAllocationResult<Self>) -> Operand {
        if address_inputs.is_empty() {
            panic!("Memory addressing instructions must have at least one input that produces the address");
        }
        let base = register_allocation.value_map
            .get(&address_inputs[0])
            .expect("Address register for MOV_LOAD should have been allocated");
        match address_inputs.len() {
            1 => Operand::MemReg(*base),
            2 => {
                let offset = address_inputs[1];
                let offset = dag.get(offset.node());
                if !offset.opcode().is_constant() {
                    panic!("Offset for MOV_LOAD with 3 inputs must be a constant (trying to encode 'mov reg, [reg + const]')");
                }

                let offset = offset.opcode().get_constant().bits_as_u64() as i32;
                Operand::MemBaseOffset(*base, offset)
            }
            _ => panic!("Memory addressing instructions may only have 1 or 2 inputs that produce the address"),
        }
    }
}

enum MemoryAddressMatch {
    /// The address is stored in the provided value, and we can't optimize it further
    Direct { value: Value },

    /// The address is computed as base +/- offset
    BasePlusOffset { base: Value, offset: Value },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Opcode {
    /// Notation:
    ///  - const: a constant value - the argument is provided by GenericOpcode::Constant
    ///  - reg:   a specific register - the argument is provided by GenericOpcode::Register
    ///  - vreg:  a virtual register - the argument is provided by any other generic or native node

    /// mov <vreg>, <const/reg/vreg>; (value) -> (value)
    MOV,

    /// mov <vreg>, [<const/reg/vreg>]; (ctrl, addr) -> (ctrl, value)
    /// mov <vreg>, [<reg/vreg> + <const>]; (ctrl, base, offset) -> (ctrl, value)
    MOV_LOAD,

    /// mov [<const/reg/vreg>], <vreg>; (ctrl, addr, value) -> (ctrl)
    /// mov [<reg/vreg> + <const>], <vreg>; (ctrl, base, offset, value) -> (ctrl)
    MOV_STORE,

    /// add <vreg>, <const/reg/vreg>; (lhs, rhs) -> (value)
    ADD,

    /// sub <vreg>, <const/reg/vreg>; (lhs, rhs) -> (value)
    SUB,

    /// ret; (ctrl, value) -> ()
    /// NOTE: the x86_64 ret instruction does not actually take any arguments, but we do take an argument here to ensure it is not eliminated by DCE
    RET,

    /// NOTE: these opcodes should not be used in the DAG, they are only used when building native instructions
    PUSH,
    POP,
}

impl Opcode {
    fn asm_name(&self) -> &'static str {
        match self {
            Opcode::MOV => "mov",
            Opcode::MOV_LOAD => "mov",
            Opcode::MOV_STORE => "mov",
            Opcode::ADD => "add",
            Opcode::SUB => "sub",
            Opcode::RET => "ret",
            Opcode::PUSH => "push",
            Opcode::POP => "pop",
        }
    }
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

            Opcode::MOV       |
            Opcode::MOV_LOAD  |
            Opcode::MOV_STORE |
            Opcode::PUSH      |
            Opcode::POP       |
            Opcode::RET       => None,
        }   
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Opcode::MOV => write!(f, "MOV"),
            Opcode::MOV_LOAD => write!(f, "MOV_LOAD"),
            Opcode::MOV_STORE => write!(f, "MOV_STORE"),
            Opcode::PUSH => write!(f, "PUSH"),
            Opcode::POP => write!(f, "POP"),
            Opcode::ADD => write!(f, "ADD"),
            Opcode::SUB => write!(f, "SUB"),
            Opcode::RET => write!(f, "RET"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    Immediate(u64),
    Register(Register),
    MemReg(Register),
    MemBaseOffset(Register, i32),
}

impl Operand {
    fn to_string(&self, ty: Type) -> String {
        match self {
            Operand::Register(reg) => reg.to_string(ty),
            Operand::Immediate(value) => ty.print_unsigned_in_hex(*value),
            Operand::MemReg(reg) => format!("[{}]", reg.to_string(Type::QWord)),
            Operand::MemBaseOffset(base, offset) => {
                if *offset >= 0 {
                    format!("[{} + {}]", base.to_string(Type::QWord), Type::DWord.print_unsigned_in_hex(*offset as u64))
                } else {
                    format!("[{} - {}]", base.to_string(Type::QWord), Type::DWord.print_unsigned_in_hex((-*offset) as u64))
                }
            }
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

    fn one_arg(opcode: Opcode, lhs: Operand, ty: Type) -> Self {
        Self {
            opcode,
            operands: [Some((lhs, ty)), None]
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
        write!(f, "{}", self.opcode.asm_name())?;
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

impl isa::NativeType for Type {
    fn size(&self) -> u32 {
        match self {
            Type::Byte => 1,
            Type::Word => 2,
            Type::DWord => 4,
            Type::QWord => 8,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Byte => write!(f, "byte"),
            Type::Word => write!(f, "word"),
            Type::DWord => write!(f, "dword"),
            Type::QWord => write!(f, "qword"),
        }
    }
}
