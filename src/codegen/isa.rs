use crate::ir;
use super::dag::*;
use super::register_allocator::RegisterAllocationResult;
use std::hash::Hash;
use std::fmt::Debug;

pub trait ISA: Sized + PartialEq {
    type Opcode : NativeOpcode;

    type Instruction : NativeInstruction;

    type Register : NativeRegister;

    type Type : NativeType;

    fn name() -> &'static str;

    fn lower_type(&self, ty: &ir::Ty) -> Self::Type;

    fn select_instruction(&self, dag: &mut DAG<Self>, instruction: NodeId);

    fn build_native_instruction(&self, dag: &DAG<Self>, instruction: &Node<Self>, register_allocation: &RegisterAllocationResult<Self>) -> Option<Self::Instruction>;

    fn get_usable_registers(&self) -> Vec<Self::Register>;
}

pub trait NativeOpcode : Copy + ToString + Eq {
    fn is_input_overwritten_by_output(&self, input: PortId) -> Option<PortId>;
}

pub trait NativeInstruction : Copy + Debug + ToString + Eq {

}

pub trait NativeRegister : Copy + Debug + ToString + Eq + Hash {

}

pub trait NativeType : Copy + Debug + ToString + Eq {

}
