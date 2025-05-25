use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::{Arc, Mutex}};
use tinyvec::ArrayVec;
use itertools::Itertools;

pub mod types;
pub mod core_dialect;

type NodePortVecIdx = u8;
pub type NodeUID = u64;
type NodeTypeUID = u32;

pub const MAX_NODE_INPUTS: usize = 16;
pub const MAX_NODE_OUTPUTS: usize = 8;

pub struct NoDebug<T>(pub T);

impl<T> std::fmt::Debug for NoDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{...}}")
    }
}

impl<T: std::fmt::Display> std::fmt::Display for NoDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum IrError {
    #[error("cannot connect `{from}` to input port `{to}` of different type")]
    CannotConnectBetweenMismatchingTypes {
        from: NoDebug<Out>,
        to: NoDebug<In>
    },

    #[error("input port `{port}` is already connected to somewhere. cannot assign using try_connect")]
    AlreadyConnected {
        port: NoDebug<In>,
    },

    #[error("not all node outputs had types inferred")]
    NotFullyTypeInferred {
        ctx: NoDebug<Node>,
    },

    #[error("the given key `{key}` does not exist in the node outputs")]
    NodeOutputNotFound {
        key: String,
        ctx: NoDebug<Node>,
    },

    #[error("Tried to clone `{ty}` that doesn't implement `core.Clone`")]
    DoesNotImplementClone {
        ty: types::Type,
        ctx: NoDebug<Node>,
    },

    #[error("Tried to disconnect type `{ty}` from `{ctx}` that doesn't implement `core.Drop`")]
    DoesNotImplementDrop {
        ty: types::Type,
        ctx: NoDebug<In>,
    },
}

pub struct DialectRegistry {
    dialects: HashMap<String, DialectRef>
}

impl DialectRegistry {
    fn new() -> Self {
        Self {
            dialects: HashMap::new()
        }
    }

    pub fn get() -> std::sync::MutexGuard<'static, Self> {
        lazy_static::lazy_static! {
            static ref REGISTRY: Mutex<DialectRegistry> = Mutex::new(DialectRegistry::new());
        }

        REGISTRY.lock().unwrap()
    }

    pub fn get_dialects(&self) -> impl Iterator<Item = (&str, DialectRef)> {
        self.dialects.iter().map(|(k,v)| (k.as_str(), v.clone()))
    }

    pub fn get_dialect(&self, key: &str) -> Option<DialectRef> {
        self.dialects.get(key).cloned()
    }

    pub(crate) fn add(&mut self, key: &str, val: DialectRef) {
        self.dialects.insert(key.to_string(), val);
    }
}

pub struct DialectBuilder(DialectRef);

impl DialectBuilder {
    pub fn new(name: &str) -> DialectBuilder {
        DialectBuilder(DialectRef::new(Dialect {
            name: name.to_string(),
            types: Mutex::new(RefCell::new(HashMap::new())),
        }))
    }

    pub fn build(self) -> DialectOwner {
        DialectRegistry::get().add(self.0.name.as_str(), self.0.clone());
        DialectOwner { dialect: self.0 }
    }

    pub fn add_type(&self, name: &str) -> types::TypeVar {
        let t = types::TypeVar::new(types::TypeVarImpl {
            dialect: self.0.clone(),
            name: name.to_string()
        });
        self.0.types.lock().unwrap().borrow_mut().insert(name.to_string(), t.clone());
        t
    }
}

pub struct Dialect {
    name: String,
    types: Mutex<RefCell<HashMap<String, types::TypeVar>>>,
}

impl PartialEq for Dialect {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Dialect {}

impl Dialect {
    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    /// you should avoid this
    pub fn find_type(&self, name: &str) -> Option<types::TypeVar> {
        self.types.lock().unwrap().borrow().get(name).cloned()
    }
}

impl std::hash::Hash for Dialect {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl std::fmt::Display for Dialect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Dialect({})", self.name)
    }
}

/// proof of ownership over a dialect
#[derive(Clone)]
pub struct DialectOwner {
    dialect: DialectRef
}

impl DialectOwner {
    pub fn get_dialect(&self) -> DialectRef {
        self.dialect.clone()
    }
}

pub type DialectRef = Arc<Dialect>;

pub struct NodeOutTypeInferRes {
    node: Node,
}

// TODO: also safer put
impl NodeOutTypeInferRes {
    fn new(node: &Node) -> Self {
        Self {
            node: node.clone()
        }
    }

    pub fn set_name(&mut self, out: &str, val: types::Type) -> Result<(), IrError> {
        let idx = self.node.get_type()
            ._lookup_output(out)
            .ok_or(IrError::NodeOutputNotFound {
                ctx: NoDebug(self.node.clone()),
                key: out.to_string()
            })?;

        self.node.0.borrow_mut().outputs[idx as usize].1 = Some(val);
        Ok(())
    }
}

pub trait NodeOutTypeInfer {
    /// this function can also serve as validation
    fn infer_outputs(&self, node: Node, out: &mut NodeOutTypeInferRes) -> Result<(), IrError>;
}

pub struct NodeTypeImpl {
  uid: NodeTypeUID,
  dialect: DialectRef,
  input_lookup: HashMap<String, (NodePortVecIdx, types::Type)>,
  output_lookup: HashMap<String, NodePortVecIdx>,
  infer: Box<dyn NodeOutTypeInfer + Send + Sync>,
  name: String,
}

impl std::hash::Hash for NodeTypeImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uid.hash(state)
    }
}

impl NodeTypeImpl {
    pub fn get_dialect(&self) -> DialectRef {
        self.dialect.clone()
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn get_inputs(&self) -> impl Iterator<Item = &str> {
        self.input_lookup.keys().map(|x| x.as_str())
    }

    pub fn get_outputs(&self) -> impl Iterator<Item = &str> {
        self.output_lookup.keys().map(|x| x.as_str())
    }

    pub(crate) fn _lookup_input(&self, key: &str) -> Option<NodePortVecIdx> {
        self.input_lookup.get(key).map(|x| x.0)
    }

    pub(crate) fn _lookup_output(&self, key: &str) -> Option<NodePortVecIdx> {
        self.output_lookup.get(key).copied()
    }

    pub(crate) fn _unlookup_input(&self, idx: NodePortVecIdx) -> Option<(&str, types::Type)> {
        self.input_lookup.iter()
            .find(|(_,(v,_))| *v == idx)
            .map(|(a,(_,b))| (a.as_str(), b.clone()))
    }

    pub(crate) fn _unlookup_output(&self, idx: NodePortVecIdx) -> Option<&str> {
        self.output_lookup.iter()
            .find(|(_,v)| **v == idx)
            .map(|(k,_)| k.as_str())
    }
}

impl std::fmt::Display for NodeTypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.dialect.get_name(), self.name)
    }
}

impl std::fmt::Debug for NodeTypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl PartialEq for NodeTypeImpl {
    fn eq(&self, other: &Self) -> bool {
        self.uid == other.uid
    }
}

pub type NodeType = Arc<NodeTypeImpl>;

pub struct NodeImpl {
    uid: NodeUID,
    typ: NodeType,
    inputs: ArrayVec<[Option<Out>; MAX_NODE_INPUTS]>,
    outputs: ArrayVec<[(Vec<In>, Option<types::Type>); MAX_NODE_OUTPUTS]>,
    inferred: bool,
}

/// clone only clones the node ref
#[derive(Clone)]
pub struct Node(pub(crate) Rc<RefCell<NodeImpl>>);

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.get_uid() == other.get_uid()
    }
}

impl Node {
    pub fn get_uid(&self) -> NodeUID {
        self.0.borrow().uid
    }

    pub fn get_type(&self) -> NodeType {
        self.0.borrow().typ.clone()
    }

    /// reference to a output port
    pub fn port_out(&self, name: &str) -> Option<Out> {
        self.0.borrow().typ._lookup_output(name)
            .map(|x| Out {
                node: self.clone(),
                idx: x,
            })
    }

    /// reference to a input port
    pub fn port_in(&self, name: &str) -> Option<In> {
        self.0.borrow().typ._lookup_input(name)
            .map(|x| In {
                node: self.clone(),
                idx: x,
            })
    }

    fn ensure_inferred(&self) -> Result<(), IrError> {
        if !self.0.borrow().inferred {
            let infer = &self.get_type().infer;
            let mut out = NodeOutTypeInferRes::new(self);
            infer.infer_outputs(self.clone(), &mut out)?;
            for (_, t) in self.0.borrow().outputs.iter() {
                if t.is_none() {
                    Err(IrError::NotFullyTypeInferred {
                        ctx: NoDebug(self.clone())
                    })?;
                }
            }
            self.0.borrow_mut().inferred = true;
        }
        Ok(())
    }
}

/// reference to a node output port
///
/// does NOT implement clone or copy!!!!
#[derive(Clone)]
pub struct Out {
    node: Node,
    idx: NodePortVecIdx
}

impl std::fmt::Display for Out {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#>{}",
            self.get_node().get_type(),
            self.get_port_name())
    }
}

impl Out {
    /// slow
    pub fn get_port_name(&self) -> String {
        self.get_node().get_type()
            ._unlookup_output(self.idx)
            .unwrap()
            .to_string()
    }

    pub fn get_node(&self) -> Node {
        self.node.clone()
    }

    /// find other node's ports that use this output port as input
    ///
    /// changes during iteration don't apply to this iterator
    pub fn find_reference_ports(&self) -> impl Iterator<Item = In> {
        self.node.0.borrow()
            .outputs[self.idx as usize]
            .0
            .clone().into_iter()
    }

    /// find other nodes that use this output port as input
    ///
    /// changes during iteration don't apply to this iterator
    pub fn find_reference_nodes(&self) -> impl Iterator<Item = Node> {
        self.find_reference_ports()
            .dedup_by(|a,b| a.node.get_uid() == b.node.get_uid())
            .map(|x| x.node)
    }

    /// tries connects this output to the given input
    ///
    /// if the given input port already is connected, this errors.
    ///
    /// if you want to overwrite the input port instead, use shr_assign (>>=)
    ///
    /// this takes ownership of the right hand side on purpose
    pub fn try_connect(&self, rhs: In) -> Result<(), IrError> {
        if rhs.find_reference_port().is_some() {
            Err(IrError::AlreadyConnected {
                port: NoDebug(rhs.clone())
            })
        } else {
            self.force_connect(rhs)
        }
    }

    pub fn get_type(&self) -> Result<types::Type, IrError> {
        self.get_node().ensure_inferred()?;

        Ok(self.node.0.borrow()
            .outputs[self.idx as usize]
            .1.clone()
            .unwrap()  // ensure_inferred already throws error if not inferred
            )
    }

    /// connects this output to the given input
    ///
    /// if the given input port already is connected, it overwrites it.
    ///
    /// this takes ownership of the right hand side on purpose
    pub fn force_connect(&self, rhs: In) -> Result<(), IrError> {
        self.get_node().ensure_inferred()?;
        let self_ty = self.get_type()?;
        if self.find_reference_ports().count() > 0 {
            if !self_ty.matches(&types::Type::var(&core_dialect::DIALECT.clone)) {
                Err(IrError::DoesNotImplementClone {
                    ty: self_ty.clone(),
                    ctx: NoDebug(self.get_node())
                })?
            }
        }
        let target_expected_type = rhs.get_type();
        if !self_ty.matches(&target_expected_type) {
            Err(IrError::CannotConnectBetweenMismatchingTypes {
                from: NoDebug(self.clone()),
                to: NoDebug(rhs.clone()),
            })?;
        }

        rhs.unchecked_disconnect();
        rhs.node.0.borrow_mut()
            .inputs[rhs.idx as usize] = Some(self.clone());
        rhs.get_node().0.borrow_mut().inferred = false;
        Ok(())
    }
}

/// reference to a node input port
///
/// does NOT implement clone or copy!!!!
#[derive(Clone, PartialEq)]
pub struct In {
    node: Node,
    idx: NodePortVecIdx
}

impl std::fmt::Display for In {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}<#{}",
            self.get_node().get_type(),
            self.get_port_name())
    }
}

impl In {
    /// slow
    pub fn get_port_name(&self) -> String {
        self.get_node().get_type()
            ._unlookup_input(self.idx)
            .unwrap().0.to_string()
    }

    pub fn get_type(&self) -> types::Type {
        self.get_node().get_type()
            ._unlookup_input(self.idx)
            .unwrap().1.clone()
    }

    /// this errors if the type doesn't implement Drop
    pub fn disconnect(&self) -> Result<(), IrError> {
        if !self.get_type().matches(&types::Type::var(&core_dialect::DIALECT.drop)) {
            Err(IrError::DoesNotImplementDrop {
                ty: self.get_type(),
                ctx: NoDebug(self.clone()),
            })?;
        }
        self.unchecked_disconnect();
        Ok(())
    }

    fn unchecked_disconnect(&self) {
        if let Some(ref_port) = self.find_reference_port() {
            self.node.0.borrow_mut()
                .inputs[self.idx as usize] = None;

            let node = ref_port.get_node();
            let mut node = node.0.borrow_mut();
            let vec = &mut node.outputs[ref_port.idx as usize].0;
            let idx = vec.iter().position(|x| x == self).unwrap();
            vec.remove(idx);
        }
    }

    pub fn get_node(&self) -> Node {
        self.node.clone()
    }

    /// find the output port that connect to this input port
    pub fn find_reference_port(&self) -> Option<Out> {
        self.node.0.borrow()
            .inputs[self.idx as usize]
            .clone()
    }
}
