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

    fn insert_register_copy(&self, dag: &mut DAG<Self>, value: Value) -> Value;

    fn build_native_instruction(&self, dag: &DAG<Self>, instruction: &Node<Self>, register_allocation: &RegisterAllocationResult<Self>) -> Option<Self::Instruction>;

    fn generate_prologue(&self) -> Vec<Self::Instruction>;

    fn generate_epilogue(&self) -> Vec<Self::Instruction>;

    fn get_usable_registers(&self) -> Vec<Self::Register>;

    fn get_stack_frame_base_register(&self) -> Self::Register;

    fn get_pointer_type(&self) -> Self::Type {
        self.lower_type(&ir::Ty::Ptr)
    }
}

pub trait NativeOpcode : Clone + ToString + Eq {
    fn is_input_overwritten_by_output(&self, input: PortId) -> Option<PortId>;
}

pub trait NativeInstruction : Clone + Debug + ToString + Eq {

}

pub trait NativeRegister : Copy + Debug + ToString + Eq + Hash {

}

pub trait NativeType : Copy + Debug + ToString + Eq {
    fn size(&self) -> u32;

    fn is_register_type(&self) -> bool;
}
