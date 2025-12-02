use crate::ir;
use super::dag::*;
use super::isa::*;
use super::opcode::*;
use std::collections::HashMap;

pub struct IRLoweringResult<I: ISA> {
    pub dag: DAG<I>,
}

pub fn lower_basic_block<I: ISA>(function: &ir::Function, bb: &ir::BasicBlock, isa: &I) -> IRLoweringResult<I> {
    let lowering = IRLowering::<I>::new(function, bb, isa);
    lowering.lower()
}

struct IRLowering<'a, I: ISA> {
    function: &'a ir::Function,
    bb: &'a ir::BasicBlock,
    isa: &'a I,
    dag: DAG<I>,
    control_token: Value,
    terminator_node: Option<NodeId>,
    lowered_values: HashMap<ir::InstructionRef, Value>,
    next_stack_offset: u32, // FIXME: this should be per function, not per BB
}

impl <'a, I: ISA> IRLowering<'a, I> {
    fn new(function: &'a ir::Function, bb: &'a ir::BasicBlock, isa: &'a I) -> Self {
        let mut dag  = DAG::new();
        let enter = dag.add_generic_node(GenericOpcode::Enter, vec![], vec![ OutputType::Control ]);
        let control_token = dag.get_value(enter, 0);
        
        Self {
            function,
            bb,
            isa,
            dag,
            control_token,
            terminator_node: None,
            lowered_values: HashMap::new(),
            next_stack_offset: 0,
        }
    }

    fn lower(mut self) -> IRLoweringResult<I> {
        for (instruction_ref, instruction) in self.bb.instructions() {
            if self.terminator_node.is_some() {
                panic!("Cannot lower IR past a return instruction");
            }
            self.lower_instruction(instruction, self.bb.get_value(instruction_ref));
        }
        self.dag.set_root(self.terminator_node.expect("Terminator node must be set during lowering"));
        IRLoweringResult {
            dag: self.dag
        }
    }

    fn lower_instruction(&mut self, instruction: &ir::Instruction, value: Option<ir::Value>) {
        match instruction {
            ir::Instruction::Add(lhs, rhs) => self.lower_arithmetic(GenericOpcode::Add, value.unwrap(), lhs, rhs),
            ir::Instruction::Sub(lhs, rhs) => self.lower_arithmetic(GenericOpcode::Sub, value.unwrap(), lhs, rhs),

            ir::Instruction::Allocate(ty) => self.lower_allocate(ty, value.unwrap()),
            ir::Instruction::Load { location, ty } => self.lower_load(location, ty, value.unwrap()),
            ir::Instruction::Store { value: store_value, location } => self.lower_store(store_value, location),

            ir::Instruction::Return(value) => self.lower_return(value),
        }
    }

    fn lower_arithmetic(&mut self, opcode: GenericOpcode<I>, ir_value: ir::Value, lhs: &ir::Value, rhs: &ir::Value) {
        let lhs = self.get_lowered_value(lhs);
        let rhs = self.get_lowered_value(rhs);
        assert!(self.dag.get_value_type(lhs) == self.dag.get_value_type(rhs), "Operands must have the same type for arithmetic operations");
        let ty = self.dag.get_value_type(lhs);
        let result = self.dag.add_generic_node(
            opcode,
            vec![
                lhs,
                rhs,
            ],
            vec![
                ty.clone()
            ]
        );
        let value = self.dag.get_value(result, 0);
        self.set_lowered_value(ir_value, value);
    }

    fn lower_allocate(&mut self, ty: &ir::Ty, ir_value: ir::Value) {
        let ty = self.isa.lower_type(ty);
        let size = ty.size();
        self.next_stack_offset += size;
        let offset = self.next_stack_offset;

        let frame_base = self.get_register(self.isa.get_stack_frame_base_register(), self.isa.get_pointer_type());
        let offset = self.get_constant(ir::Constant::int(-(offset as i32)));
        let addr = self.dag.add_generic_node(
            GenericOpcode::Add,
            vec![
                frame_base,
                offset,
            ],
            vec![
                self.get_pointer_type()
            ]
        );
        let addr = self.dag.get_value(addr, 0);
        self.set_lowered_value(ir_value, addr);
    }

    fn lower_load(&mut self, location: &ir::Value, ty: &ir::Ty, ir_value: ir::Value) {
        let location = self.get_lowered_value(location);
        let ty = self.isa.lower_type(ty);
        let load = self.dag.add_generic_node(
            GenericOpcode::Load,
            vec![
                self.control_token,
                location,
            ],
            vec![
                OutputType::Control,
                OutputType::Native(ty)
            ]
        );
        let value = self.dag.get_value(load, 1);
        self.control_token = self.dag.get_value(load, 0);
        self.set_lowered_value(ir_value, value);
    }

    fn lower_store(&mut self, value: &ir::Value, location: &ir::Value) {
        let value = self.get_lowered_value(value);
        let location = self.get_lowered_value(location);
        let store = self.dag.add_generic_node(
            GenericOpcode::Store,
            vec![
                self.control_token,
                location,
                value,
            ],
            vec![
                OutputType::Control
            ]
        );
        self.control_token = self.dag.get_value(store, 0);
    }

    fn lower_return(&mut self, value: &ir::Value) {
        let value = self.get_lowered_value(value);
        let ret = self.dag.add_generic_node(
            GenericOpcode::Ret,
            vec![
                self.control_token,
                value,
            ],
            vec![]
        );
        self.terminator_node = Some(ret);
    }

    fn get_lowered_value(&mut self, value: &ir::Value) -> Value {
        match value {
            ir::Value::Constant(c) => self.get_constant(c.clone()),
            ir::Value::Instruction(instruction_ref, name) => {
                if let Some(lowered_value) = self.lowered_values.get(instruction_ref) {
                    lowered_value.clone()
                } else {
                    panic!("Lowered value for IR value %{} not found", name);
                }
            }
        }
    }

    fn set_lowered_value(&mut self, ir_value: ir::Value, lowered_value: Value) {
        let (instruction_ref, name) = match ir_value {
            ir::Value::Instruction(instruction_ref, name) => (instruction_ref, name),
            _ => panic!("Cannot set lowered value for non-instruction value"),
        };
        assert!(!self.lowered_values.contains_key(&instruction_ref), "Lowered value for IR value %{} already exists", name);
        self.lowered_values.insert(instruction_ref, lowered_value);
    }

    fn get_constant(&mut self, value: ir::Constant) -> Value {
        let ty = self.isa.lower_type(value.ty());
        let constant = self.dag.add_generic_node(
            GenericOpcode::Constant(value.clone()),
            vec![],
            vec![ OutputType::Native(ty) ]
        );
        self.dag.get_value(constant, 0)
    }

    fn get_register(&mut self, reg: I::Register, ty: I::Type) -> Value {
        let register = self.dag.add_generic_node(
            GenericOpcode::Register(reg),
            vec![],
            vec![ OutputType::Native(ty) ]
        );
        self.dag.get_value(register, 0)
    }

    fn get_pointer_type(&self) -> OutputType<I> {
        OutputType::Native(self.isa.get_pointer_type())
    }
}