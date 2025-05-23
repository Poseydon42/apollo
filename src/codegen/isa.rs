use crate::ir;
use super::dag::*;
use super::register_allocator::RegisterAllocationResult;
use std::hash::Hash;

pub trait ISA: Sized + PartialEq {
    type Opcode : NativeOpcode;

    type Instruction : NativeInstruction;

    type Register : NativeRegister;

    type Type : NativeType;

    fn name() -> &'static str;

    fn lower_type(&self, ty: &ir::Ty) -> Self::Type;

    fn select_instruction(&self, dag: &mut DAG<Self>, instruction: NodeId);

    fn build_native_instruction(&self, instruction: &Node<Self>, register_allocation: &RegisterAllocationResult<Self>) -> Option<Self::Instruction>;

    fn get_usable_registers(&self) -> Vec<Self::Register>;
}

pub trait NativeOpcode : Copy + ToString + Eq {

}

pub trait NativeInstruction : Copy + ToString + Eq {

}

pub trait NativeRegister : Copy + ToString + Eq + Hash {

}

pub trait NativeType : Copy + ToString + Eq {

}
