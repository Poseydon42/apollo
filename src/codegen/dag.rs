use super::isa::*;
use super::opcode::*;
use core::panic;
use std::collections::HashSet;
use std::fmt::*;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeId(usize);

pub type PortId = usize;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Value(NodeId, PortId);

impl Value {
    pub fn node(&self) -> NodeId {
        self.0
    }

    pub fn port(&self) -> PortId {
        self.1
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "#{}:{}", self.0.0, self.1)
    }
}

pub struct Node<I: ISA> {
    id: NodeId,
    opcode: Opcode<I>,
    inputs: Vec<NodeInput<I>>,
    outputs: Vec<NodeOutput<I>>,
}

impl <I: ISA> Node<I> {
    pub fn id(&self) -> NodeId {
        self.id
    }

    pub fn opcode(&self) -> &Opcode<I> {
        &self.opcode
    }

    pub fn is_generic(&self) -> bool {
        match self.opcode {
            Opcode::Generic(_) => true,
            Opcode::Native(_) => false,
        }
    }

    pub fn is_native(&self) -> bool {
        !self.is_generic()
    }

    pub fn inputs(&self) -> impl DoubleEndedIterator<Item = &Value> {
        self.inputs.iter().map(|input| &input.value)
    }
    
    pub fn get_input(&self, input: PortId) -> Value {
        assert!(
            input < self.inputs.len(),
            "Trying to access input {} of node {}, but the node has only {} inputs",
            input, self.id.0, self.inputs.len());
        self.inputs[input].value
    }

    pub fn is_input_allowed_in_register(&self, input: PortId, reg: I::Register) -> bool {
        assert!(
            input < self.inputs.len(),
            "Trying to access input {} of node {}, but the node has only {} inputs",
            input, self.id.0, self.inputs.len());
        if self.inputs[input].allowed_registers.is_empty() {
            return true;
        }
        self.inputs[input].allowed_registers.iter().any(|allowed| *allowed == reg)
    }

    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = Value> + use<'_, I> {
        (0..self.output_count()).map(|port| self.get_output(port))
    }

    pub fn output_count(&self) -> usize {
        self.outputs.len()
    }

    pub fn get_output(&self, output: PortId) -> Value {
        assert!(
            output < self.outputs.len(),
            "Trying to access output {} of node {}, but the node has only {} outputs",
            output, self.id.0, self.outputs.len());
        Value(self.id, output)
    }

    pub fn get_output_type(&self, output: PortId) -> &OutputType<I> {
        assert!(
            output < self.outputs.len(),
            "Trying to access output {} of node {}, but the node has only {} outputs",
            output, self.id.0, self.outputs.len());
        &self.outputs[output].ty
    }

    pub fn uses<'a>(&self, port: PortId) -> impl Iterator<Item = &(NodeId, PortId)> {
        assert!(
            port < self.outputs.len(),
            "Trying to access output {} of node {}, but the node has only {} outputs",
            port, self.id.0, self.outputs.len());
        self.outputs[port].uses.iter()
    }

    pub fn is_dead(&self) -> bool {
        self.outputs.iter().all(|output| output.uses.is_empty())
    }
}

impl <I: ISA> ToString for Node<I> {
    fn to_string(&self) -> String {
        let mut result = format!("#{}: {}", self.id.0, self.opcode.to_string());

        result.push_str(": (");
        for input in &self.inputs {
            result.push_str(format!("#{}:{}, ", input.value.0.0, input.value.1).as_str());
        }
        if self.inputs.len() > 0 {
            result.pop(); // remove last comma
            result.pop(); // remove last space
        }
        result.push_str(") -> (");
        for output in &self.outputs {
            result.push_str(format!("{}, ", output.ty).as_str());
        }
        if self.outputs.len() > 0 {
            result.pop(); // remove last comma
            result.pop(); // remove last space
        }
        result.push(')');
        result
    }
}

#[derive(Copy, PartialEq)]
pub enum OutputType<I: ISA> {
    Control,
    Native(I::Type),
}

impl <I: ISA> OutputType<I> {
    pub fn get_native(&self) -> &I::Type {
        match self {
            OutputType::Control => panic!("Output type is not native"),
            OutputType::Native(ty) => ty,
        }
    }
}

impl <I: ISA> Clone for OutputType<I> {
    fn clone(&self) -> Self {
        match self {
            OutputType::Control => OutputType::Control,
            OutputType::Native(ty) => OutputType::Native(ty.clone()),
        }
    }
}

impl <I: ISA> Display for OutputType<I> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            OutputType::Control => write!(f, "ctrl"),
            OutputType::Native(ty) => write!(f, "{}::{}", I::name(), ty.to_string()),
        }
    }
}

struct NodeOutput<I: ISA> {
    ty: OutputType<I>,
    uses: HashSet<(NodeId, PortId)>,
}

impl <I: ISA> Clone for NodeOutput<I>  {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            uses: self.uses.clone(),
        }
    }
}

struct NodeInput<I: ISA> {
    value: Value,
    allowed_registers: Vec<I::Register>,
}

pub struct DAG<I: ISA> {
    nodes: Vec<Node<I>>,
    root: Option<NodeId>,
}

impl <I: ISA> DAG<I> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            root: None,
        }
    }

    pub fn null_value() -> Value {
        Value(NodeId(usize::MAX), usize::MAX)
    }

    pub fn add_generic_node(&mut self, opcode: GenericOpcode<I>, inputs: Vec<Value>, outputs: Vec<OutputType<I>>) -> NodeId {
        self.add_node(Opcode::Generic(opcode), inputs, outputs)
    }

    pub fn add_native_node(&mut self, opcode: I::Opcode, inputs: Vec<Value>, outputs: Vec<OutputType<I>>) -> NodeId {
        self.add_node(Opcode::native(opcode), inputs, outputs)
    }

    pub fn get(&self, id: NodeId) -> &Node<I> {
        &self.nodes[id.0]
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node<I>> {
        self.nodes.iter()
    }

    pub fn root(&self) -> Option<NodeId> {
        self.root
    }

    pub fn set_root(&mut self, root: NodeId) {
        assert!(root.0 < self.nodes.len(), "Node ID out of bounds");
        self.root = Some(root);
    }

    pub fn get_value(&self, node: NodeId, port: PortId) -> Value {
        self.get(node).get_output(port)
    }

    pub fn get_value_type(&self, value: Value) -> &OutputType<I> {
        self.get(value.node()).get_output_type(value.port())
    }

    pub fn uses(&self, value: Value) -> impl Iterator<Item = &(NodeId, PortId)> {
        let node = self.get(value.node());
        node.uses(value.port())
    }

    pub fn set_input(&mut self, node_id: NodeId, port: PortId, value: Value) {
        let node = self.get_mut(node_id);
        assert!(port < node.inputs.len(), "Input index out of bounds for node {}", node_id.0);
        assert!(value.node() != node_id, "Cannot set input to self");

        let old_value = node.get_input(port);
        if old_value == value {
            return;
        }

        node.inputs[port].value = value;

        let old_node = self.get_mut(old_value.node());
        old_node.outputs[old_value.port()].uses.remove(&(node_id, port));

        let new_node = self.get_mut(value.node());
        new_node.outputs[value.port()].uses.insert((node_id, port));
    }

    pub fn set_allowed_registers(&mut self, node_id: NodeId, port: PortId, registers: Vec<I::Register>) {
        let node = self.get_mut(node_id);
        assert!(port < node.inputs.len(), "Input index out of bounds for node {}", node_id.0);
        node.inputs[port].allowed_registers = registers;
    }

    pub fn replace_node(&mut self, old_id: NodeId, new_id: NodeId) {
        if old_id == new_id {
            return;
        }
        assert!(self.get(old_id).outputs.len() == self.get(new_id).outputs.len(), "Cannot replace node with different number of outputs");
        for (port, output) in self.get(old_id).outputs.iter().enumerate() {
            assert!(output.ty == *self.get(new_id).get_output_type(port), "Cannot replace node with different output type {}", port);
        }

        let old_node_outputs = self.get(old_id).outputs.clone(); // Making a copy here to shut the borrow checker up
        for (port, output) in old_node_outputs.iter().enumerate() {
            for (user_id, user_port) in &output.uses {
                // NOTE: this is so that the old node can still be used as the input of the new node
                if *user_id == new_id {
                    continue;
                }
                self.set_input(*user_id, *user_port, self.get_value(new_id, port));
            }
        }

        if self.root == Some(old_id) {
            self.root = Some(new_id);
        }
    }
    
    fn add_node(&mut self, opcode: Opcode<I>, inputs: Vec<Value>, outputs: Vec<OutputType<I>>) -> NodeId {
        let id = NodeId(self.nodes.len());
        for (port, value) in inputs.iter().enumerate() {
            self.nodes[value.node().0].outputs[value.port()].uses.insert((id, port));
        }
        let inputs = inputs.into_iter().map(|value| NodeInput { value, allowed_registers: Vec::new() }).collect();
        self.nodes.push(Node {
            id,
            opcode,
            inputs,
            outputs: outputs.into_iter().map(|ty| NodeOutput { ty, uses: HashSet::new() }).collect(),
        });
        id
    }
    
    fn get_mut(&mut self, id: NodeId) -> &mut Node<I> {
        &mut self.nodes[id.0]
    }
}
