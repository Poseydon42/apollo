use crate::ir;
use super::dag::*;
use super::isa::ISA;
use super::opcode::*;

pub struct IRLoweringResult<I: ISA> {
    pub dag: DAG<I>,
}

pub fn lower_function<I: ISA>(function: &ir::Function, isa: &I) -> IRLoweringResult<I> {
    let lowering = IRLowering::<I>::new(function, isa);
    lowering.lower()
}

struct IRLowering<'a, I: ISA> {
    function: &'a ir::Function,
    isa: &'a I,
    dag: DAG<I>,
    control_token: Value,
    terminator_node: Option<NodeId>,
}

impl <'a, I: ISA> IRLowering<'a, I> {
    fn new(function: &'a ir::Function, isa: &'a I) -> Self {
        let mut dag  = DAG::new();
        let enter = dag.add_generic_node(GenericOpcode::Enter, vec![], vec![ OutputType::Control ]);
        let control_token = dag.get_value(enter, 0);
        
        Self {
            function,
            isa,
            dag,
            control_token,
            terminator_node: None,
        }
    }

    fn lower(mut self) -> IRLoweringResult<I> {
        for (_ref, instruction) in self.function.code.instructions() {
            if self.terminator_node.is_some() {
                panic!("Cannot lower IR past a return instruction");
            }
            self.lower_instruction(instruction);
        }
        self.dag.set_root(self.terminator_node.expect("Terminator node must be set during lowering"));
        IRLoweringResult {
            dag: self.dag
        }
    }

    fn lower_instruction(&mut self, instruction: &ir::Instruction) {
        match instruction {
            ir::Instruction::Return(value) => self.lower_return(value),
            _ => panic!(),
        }
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
            ir::Value::Instruction(_, _) => todo!(),
        }
    }

    fn get_constant(&mut self, value: ir::Constant) -> Value {
        let ty = self.isa.lower_type(value.ty());
        let constant = self.dag.add_generic_node_with_payload(
            GenericOpcode::Constant,
            NodePayload::Constant(value), 
            vec![],
            vec![ OutputType::Native(ty) ]
        );
        self.dag.get_value(constant, 0)
    }
}