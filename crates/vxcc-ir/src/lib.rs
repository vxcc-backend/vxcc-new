use itertools::Itertools;
pub use serde_json::Value;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
    sync::{Arc, LazyLock, Mutex},
};

pub mod types;
pub mod vxcc_core_dialect;

#[cfg(test)]
mod types_tests;

pub type NodePortVecIdx = u8;
pub type NodeUID = quid::UID;
type NodeTypeUID = quid::UID;

pub struct NoDebug<T>(pub T);

impl<T> std::fmt::Debug for NoDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{...}}")
    }
}

impl<T: std::fmt::Display> std::fmt::Display for NoDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum IrError {
    CannotConnectBetweenMismatchingTypes { from: NoDebug<Out>, to: NoDebug<In> },

    AlreadyConnected { port: NoDebug<In> },

    NotFullyTypeInferred { ctx: NoDebug<Node> },

    NodeOutputNotFound { key: String, ctx: NoDebug<NodeType> },

    NodeInputNotFound { key: String, ctx: NoDebug<NodeType> },

    NodeAttrNotFound { key: String, ctx: NoDebug<NodeType> },

    DoesNotImplementClone { ty: types::Type, ctx: NoDebug<Node> },

    DoesNotImplementDrop { ty: types::Type, ctx: NoDebug<In> },

    ImpliesRhsIsNotLegal { rhs: types::Type },

    ImpliesLhsIsNotLegal { lhs: types::Type },

    TypeError(types::TypeError),

    MissingNodeInputs { ctx: NoDebug<NodeType> },
}

impl From<types::TypeError> for IrError {
    fn from(value: types::TypeError) -> Self {
        Self::TypeError(value)
    }
}

impl std::fmt::Display for IrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrError::CannotConnectBetweenMismatchingTypes { from, to } => write!(
                f,
                "cannot connect `{from}` to input port `{to}` of different type"
            ),
            IrError::AlreadyConnected { port } => write!(
                f,
                "input port `{port}` is already connected to somewhere. cannot assign using try_connect"
            ),
            IrError::NotFullyTypeInferred { ctx: _ } => {
                write!(f, "not all node outputs had types inferred")
            }
            IrError::NodeOutputNotFound { key, ctx } => write!(
                f,
                "the given key `{key}` does not exist in outputs of {ctx}"
            ),
            IrError::NodeInputNotFound { key, ctx } => {
                write!(f, "the given key `{key}` does not exist in inputs of {ctx}")
            }
            IrError::NodeAttrNotFound { key, ctx } => write!(
                f,
                "the given key `{key}` does not exist in attributes of {ctx}"
            ),
            IrError::MissingNodeInputs { ctx } => {
                write!(f, "missing inputs while constructing node of type `{ctx}`")
            }
            IrError::DoesNotImplementClone { ty, ctx: _ } => write!(
                f,
                "Tried to clone `{ty}` that doesn't implement `core.Clone`"
            ),
            IrError::DoesNotImplementDrop { ty, ctx } => write!(
                f,
                "Tried to disconnect type `{ty}` from `{ctx}` that doesn't implement `core.Drop`"
            ),
            IrError::ImpliesRhsIsNotLegal { rhs } => write!(
                f,
                "Right hand side of implies expression is not legal: `{rhs}`"
            ),
            IrError::ImpliesLhsIsNotLegal { lhs } => write!(
                f,
                "Left hand side of implies expression is not legal: `{lhs}`"
            ),
            IrError::TypeError(type_error) => write!(f, "{type_error}"),
        }
    }
}

pub struct DialectRegistry {
    dialects: HashMap<String, DialectRef>,
    lateinit: Vec<fn()>,
}

impl DialectRegistry {
    fn new() -> Self {
        Self {
            dialects: HashMap::new(),
            lateinit: vec![],
        }
    }

    fn get() -> std::sync::MutexGuard<'static, Self> {
        static REGISTRY: LazyLock<Mutex<DialectRegistry>> =
            LazyLock::new(|| Mutex::new(DialectRegistry::new()));

        let mut lock = REGISTRY.lock().unwrap();
        for func in lock.lateinit.drain(..) {
            func();
        }

        lock
    }

    pub fn get_dialects() -> Vec<(String, DialectRef)> {
        Self::get()
            .dialects
            .iter()
            .map(|(k, v)| (k.as_str().to_string(), v.clone()))
            .collect()
    }

    pub fn get_dialect(key: &str) -> Option<DialectRef> {
        Self::get().dialects.get(key).cloned()
    }

    pub(crate) fn add(key: &str, val: DialectRef) {
        Self::get().dialects.insert(key.to_string(), val);
    }
}

pub struct DialectDep {
    pub optional: bool,
    pub name: String,
}

pub struct DialectBuilder(DialectRef);

impl DialectBuilder {
    pub fn new(name: &str) -> DialectBuilder {
        let r = DialectRef::new(Dialect {
            name: name.to_string(),
            types: Mutex::new(RefCell::new(HashMap::new())),
            node_types: Mutex::new(RefCell::new(HashMap::new())),
        });
        DialectRegistry::add(name, r.clone());
        DialectBuilder(r)
    }

    pub fn dont_call_this_add_lateinit(&self, func: fn()) {
        DialectRegistry::get().lateinit.push(func);
    }

    pub fn wip_ref(&self) -> DialectRef {
        self.0.clone()
    }

    pub fn build(self) -> DialectOwner {
        DialectOwner { dialect: self.0 }
    }

    pub fn add_type(&mut self, name: &str) -> types::TypeVar {
        self.add_ground_type(name, std::iter::empty::<String>())
    }

    pub fn add_ground_type<I: Iterator<Item = S>, S: AsRef<str>>(
        &mut self,
        name: &str,
        params: I,
    ) -> types::TypeVar {
        let t = types::TypeVar::new(types::TypeVarImpl {
            dialect: self.0.clone(),
            name: name.to_string(),
            ground_args: params.map(|x| x.as_ref().to_string()).collect(),
        });
        self.0
            .types
            .lock()
            .unwrap()
            .borrow_mut()
            .insert(name.to_string(), t.clone());
        t
    }

    /// in the future, implies have to have types you own on one of the sides
    ///
    /// the right side can only be Var or Ground
    pub fn add_implies(&mut self, from: types::Type, to: types::Type) -> Result<(), IrError> {
        match &*to.0 {
            types::TypeImpl::Any | types::TypeImpl::And(_) | types::TypeImpl::Unspec(_) => {
                Err(IrError::ImpliesRhsIsNotLegal { rhs: to.clone() })?;
            }

            _ => {}
        }

        let varlist = from.to_var_list();
        if varlist.is_empty() {
            Err(IrError::ImpliesLhsIsNotLegal { lhs: from.clone() })?;
        }

        for item in varlist.into_iter() {
            types::Type::denorm_add(item, from.clone(), to.clone());
        }

        Ok(())
    }

    pub fn add_node_type(
        &mut self,
        name: impl AsRef<str>,
        ty: types::Type,
        infer: Box<dyn NodeOutTypeInfer + Send + Sync>,
        args: impl Iterator<Item = (impl AsRef<str>, types::Type)>,
        outs: impl Iterator<Item = impl AsRef<str>>,
        attrs: impl Iterator<Item = impl AsRef<str>>,
    ) -> Result<NodeType, IrError> {
        let ty = NodeTypeImpl {
            runtime_uid: quid::UID::new(),
            dialect: self.0.clone(),
            input_lookup: args
                .enumerate()
                .map(|(i, (n, t))| (n.as_ref().to_string(), (i as NodePortVecIdx, t)))
                .collect(),
            output_lookup: outs
                .enumerate()
                .map(|(i, v)| (v.as_ref().to_string(), i as NodePortVecIdx))
                .collect(),
            infer,
            name: name.as_ref().to_string(),
            attrs_lookup: attrs
                .enumerate()
                .map(|(k, v)| (v.as_ref().to_string(), k as u8))
                .collect(),
            ty,
        };
        let ty = NodeType::new(ty);

        self.0
            .node_types
            .lock()
            .unwrap()
            .borrow_mut()
            .insert(name.as_ref().to_string(), ty.clone());

        Ok(ty)
    }
}

pub struct Dialect {
    name: String,
    // mutex here is useless but DialectBuilder needs it and I don't want to waste time figureing
    // out how to fix
    types: Mutex<RefCell<HashMap<String, types::TypeVar>>>,
    node_types: Mutex<RefCell<HashMap<String, NodeType>>>,
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
    pub fn find_type<S: AsRef<str>>(&self, name: S) -> Option<types::TypeVar> {
        self.types
            .lock()
            .unwrap()
            .borrow()
            .get(name.as_ref())
            .cloned()
    }

    /// you should avoid this
    pub fn find_node_type<S: AsRef<str>>(&self, name: S) -> Option<NodeType> {
        self.node_types
            .lock()
            .unwrap()
            .borrow()
            .get(name.as_ref())
            .cloned()
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
    dialect: DialectRef,
}

impl DialectOwner {
    pub fn get_dialect(&self) -> DialectRef {
        self.dialect.clone()
    }
}

#[cfg(feature = "quote")]
impl quote::ToTokens for Dialect {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::quote;

        let name = self.get_name();
        tokens.extend(quote! { vxcc_ir::DialectRegistry::get().get_dialect(#name).unwrap() })
    }
}

pub type DialectRef = Arc<Dialect>;

pub struct NodeOutTypeInferRes {
    node: Node,
}

// TODO: also safer set_name
impl NodeOutTypeInferRes {
    fn new(node: &Node) -> Self {
        Self { node: node.clone() }
    }

    pub fn set_name(&mut self, out: &str, val: types::Type) -> Result<(), IrError> {
        let idx = self
            .node
            .get_type()
            ._lookup_output(out)
            .ok_or(IrError::NodeOutputNotFound {
                key: out.to_string(),
                ctx: NoDebug(self.node.get_type().clone()),
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
    /// only used for faster hashing at runtime
    runtime_uid: NodeTypeUID,
    dialect: DialectRef,
    input_lookup: HashMap<String, (NodePortVecIdx, types::Type)>,
    output_lookup: HashMap<String, NodePortVecIdx>,
    attrs_lookup: HashMap<String, u8>,
    infer: Box<dyn NodeOutTypeInfer + Send + Sync>,
    name: String,
    ty: types::Type,
}

impl std::hash::Hash for NodeTypeImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_temporary_uid().hash(state)
    }
}

impl NodeTypeImpl {
    /// will change during IR re-serialization
    pub fn get_temporary_uid(&self) -> NodeTypeUID {
        self.runtime_uid.clone()
    }

    pub fn get_dialect(&self) -> DialectRef {
        self.dialect.clone()
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn num_inputs(&self) -> usize {
        self.input_lookup.len()
    }

    pub fn num_outputs(&self) -> usize {
        self.output_lookup.len()
    }

    pub fn num_attrs(&self) -> usize {
        self.attrs_lookup.len()
    }

    pub fn get_inputs(&self) -> impl Iterator<Item = &str> {
        self.input_lookup.keys().map(|x| x.as_str())
    }

    pub fn get_outputs(&self) -> impl Iterator<Item = &str> {
        self.output_lookup.keys().map(|x| x.as_str())
    }

    pub fn get_attrs(&self) -> impl Iterator<Item = &str> {
        self.attrs_lookup.keys().map(|x| x.as_str())
    }

    pub fn get_type(&self) -> types::Type {
        self.ty.clone()
    }

    pub(crate) fn _lookup_input(&self, key: &str) -> Option<NodePortVecIdx> {
        self.input_lookup.get(key).map(|x| x.0)
    }

    pub(crate) fn _lookup_output(&self, key: &str) -> Option<NodePortVecIdx> {
        self.output_lookup.get(key).copied()
    }

    pub(crate) fn _lookup_attr(&self, key: &str) -> Option<u8> {
        self.attrs_lookup.get(key).copied()
    }

    pub(crate) fn _unlookup_input(&self, idx: NodePortVecIdx) -> Option<(&str, types::Type)> {
        self.input_lookup
            .iter()
            .find(|(_, (v, _))| *v == idx)
            .map(|(a, (_, b))| (a.as_str(), b.clone()))
    }

    pub(crate) fn _unlookup_output(&self, idx: NodePortVecIdx) -> Option<&str> {
        self.output_lookup
            .iter()
            .find(|(_, v)| **v == idx)
            .map(|(k, _)| k.as_str())
    }

    pub(crate) fn _unlookup_attr(&self, idx: u8) -> Option<&str> {
        self.attrs_lookup
            .iter()
            .find(|(_, v)| **v == idx)
            .map(|(k, _)| k.as_str())
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
        self.get_temporary_uid() == other.get_temporary_uid()
    }
}

pub type NodeType = Arc<NodeTypeImpl>;

// type NodeInoutVec<T> = ArrayVec<[T; 8]>;
type NodeInoutVec<T> = Vec<T>;

pub struct NodeImpl {
    uid: NodeUID,
    typ: NodeType,
    inputs: NodeInoutVec<Option<Out>>,
    outputs: NodeInoutVec<(Vec<In>, Option<types::Type>)>,
    inferred: bool,
    attrs: Vec<Value>,
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
    pub fn new(
        ty: &NodeType,
        inputs: impl Iterator<Item = (impl AsRef<str>, Out)>,
        attrs: impl Iterator<Item = (impl AsRef<str>, Value)>,
    ) -> Result<Self, IrError> {
        let mut ins = NodeInoutVec::new();
        ins.resize(ty.num_inputs(), None);

        for (k, v) in inputs {
            let idx = ty
                ._lookup_input(k.as_ref())
                .ok_or(IrError::NodeInputNotFound {
                    key: k.as_ref().to_string(),
                    ctx: NoDebug(ty.clone()),
                })?;
            ins[idx as usize] = Some(v);
        }

        if !ins.iter().all(|x| x.is_some()) {
            Err(IrError::MissingNodeInputs {
                ctx: NoDebug(ty.clone()),
            })?
        }

        let mut a = Vec::new();
        a.resize(ty.num_attrs(), Value::Null);

        for (k, v) in attrs {
            let idx = ty
                ._lookup_attr(k.as_ref())
                .ok_or(IrError::NodeAttrNotFound {
                    key: k.as_ref().to_string(),
                    ctx: NoDebug(ty.clone()),
                })?;
            a[idx as usize] = v;
        }

        let mut outputs = NodeInoutVec::new();
        outputs.resize(ty.num_outputs(), (vec![], None));

        let node = Self(Rc::new(RefCell::new(NodeImpl {
            uid: NodeUID::new(),
            typ: ty.clone(),
            inputs: ins,
            outputs,
            inferred: false,
            attrs: a,
        })));

        node.ensure_inferred()?;

        Ok(node)
    }

    pub fn get_uid(&self) -> NodeUID {
        self.0.borrow().uid.clone()
    }

    pub fn get_type(&self) -> NodeType {
        self.0.borrow().typ.clone()
    }

    pub fn ports_out(&self) -> Vec<Out> {
        self.0
            .borrow()
            .outputs
            .iter()
            .enumerate()
            .map(|(i, _)| unsafe { Out::new(self.clone(), i as u8) })
            .collect()
    }

    pub fn ports_in(&self) -> Vec<In> {
        self.0
            .borrow()
            .inputs
            .iter()
            .enumerate()
            .map(|(i, _)| unsafe { In::new(self.clone(), i as u8) })
            .collect()
    }

    /// reference to a output port
    pub fn port_out(&self, name: &str) -> Option<Out> {
        self.0
            .borrow()
            .typ
            ._lookup_output(name)
            .map(|x| unsafe { Out::new(self.clone(), x) })
    }

    /// reference to a input port
    pub fn port_in(&self, name: &str) -> Option<In> {
        self.0
            .borrow()
            .typ
            ._lookup_input(name)
            .map(|x| unsafe { In::new(self.clone(), x) })
    }

    pub fn attr(&self, name: &str) -> Option<Value> {
        self.0
            .borrow()
            .typ
            ._lookup_attr(name)
            .map(|x| self.0.borrow().attrs[x as usize].clone())
    }

    fn ensure_inferred(&self) -> Result<(), IrError> {
        if !self.0.borrow().inferred {
            let infer = &self.get_type().infer;
            let mut out = NodeOutTypeInferRes::new(self);
            infer.infer_outputs(self.clone(), &mut out)?;
            for (_, t) in self.0.borrow().outputs.iter() {
                if t.is_none() {
                    Err(IrError::NotFullyTypeInferred {
                        ctx: NoDebug(self.clone()),
                    })?;
                }
            }
            self.0.borrow_mut().inferred = true;
        }
        Ok(())
    }

    /// sometimes used by vxcc_dialect_parser::cast
    pub fn dyn_cast(&self, dialect: impl AsRef<str>, name: impl AsRef<str>) -> Option<Node> {
        let ty = self.get_type();
        if ty.dialect.name == dialect.as_ref() && ty.name == name.as_ref() {
            Some(self.clone())
        } else {
            None
        }
    }
}

/// reference to a node output port
///
/// does NOT implement clone or copy!!!!
#[derive(Clone)]
pub struct Out {
    node: Node,
    idx: NodePortVecIdx,
}

impl std::fmt::Display for Out {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}#>{}",
            self.get_node().get_type(),
            self.get_port_name()
        )
    }
}

impl Out {
    pub unsafe fn new(node: Node, idx: NodePortVecIdx) -> Self {
        Self { node, idx }
    }

    /// slow
    pub fn get_port_name(&self) -> String {
        self.get_node()
            .get_type()
            ._unlookup_output(self.idx)
            .unwrap()
            .to_string()
    }

    pub fn get_index(&self) -> usize {
        self.idx as usize
    }

    pub fn get_node(&self) -> Node {
        self.node.clone()
    }

    /// find other node's ports that use this output port as input
    ///
    /// changes during iteration don't apply to this iterator
    pub fn find_reference_ports(&self) -> impl Iterator<Item = In> {
        unsafe {
            self.node
                .0
                .borrow()
                .outputs
                .get_unchecked(self.idx as usize)
                .0
                .clone()
                .into_iter()
        }
    }

    /// find other nodes that use this output port as input
    ///
    /// changes during iteration don't apply to this iterator
    pub fn find_reference_nodes(&self) -> impl Iterator<Item = Node> {
        self.find_reference_ports()
            .dedup_by(|a, b| a.node.get_uid() == b.node.get_uid())
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
                port: NoDebug(rhs.clone()),
            })
        } else {
            self.force_connect(rhs)
        }
    }

    pub fn get_type(&self) -> Result<types::Type, IrError> {
        self.get_node().ensure_inferred()?;

        unsafe {
            Ok(
                self.node
                    .0
                    .borrow()
                    .outputs
                    .get_unchecked(self.idx as usize)
                    .1
                    .clone()
                    .unwrap_unchecked(), // ensure_inferred already throws error if not inferred
            )
        }
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
            if !self_ty.matches(&types::Type::var(&vxcc_core_dialect::DIALECT.types.Clone))? {
                Err(IrError::DoesNotImplementClone {
                    ty: self_ty.clone(),
                    ctx: NoDebug(self.get_node()),
                })?
            }
        }
        let target_expected_type = rhs.get_type();
        if !self_ty.matches(&target_expected_type)? {
            Err(IrError::CannotConnectBetweenMismatchingTypes {
                from: NoDebug(self.clone()),
                to: NoDebug(rhs.clone()),
            })?;
        }

        rhs.unchecked_disconnect();
        unsafe {
            *rhs.node
                .0
                .borrow_mut()
                .inputs
                .get_unchecked_mut(rhs.idx as usize) = Some(self.clone());
        }
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
    idx: NodePortVecIdx,
}

impl std::fmt::Display for In {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}<#{}",
            self.get_node().get_type(),
            self.get_port_name()
        )
    }
}

impl In {
    pub unsafe fn new(node: Node, idx: NodePortVecIdx) -> Self {
        Self { node, idx }
    }

    /// slow
    pub fn get_port_name(&self) -> String {
        self.get_node()
            .get_type()
            ._unlookup_input(self.idx)
            .unwrap()
            .0
            .to_string()
    }

    pub fn get_index(&self) -> usize {
        self.idx as usize
    }

    pub fn get_type(&self) -> types::Type {
        self.get_node()
            .get_type()
            ._unlookup_input(self.idx)
            .unwrap()
            .1
            .clone()
    }

    /// this errors if the type doesn't implement Drop
    pub fn disconnect(&self) -> Result<(), IrError> {
        if !self
            .get_type()
            .matches(&types::Type::var(&vxcc_core_dialect::DIALECT.types.Drop))?
        {
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
            unsafe {
                *self
                    .node
                    .0
                    .borrow_mut()
                    .inputs
                    .get_unchecked_mut(self.idx as usize) = None;
            }

            let node = ref_port.get_node();
            let mut node = node.0.borrow_mut();
            let vec = unsafe { &mut node.outputs.get_unchecked_mut(ref_port.idx as usize).0 };
            let idx = vec.iter().position(|x| x == self).unwrap();
            vec.remove(idx);
        }
    }

    pub fn get_node(&self) -> Node {
        self.node.clone()
    }

    /// find the output port that connect to this input port
    pub fn find_reference_port(&self) -> Option<Out> {
        unsafe {
            self.node
                .0
                .borrow()
                .inputs
                .get_unchecked(self.idx as usize)
                .clone()
        }
    }
}

pub fn dump(node: Node) -> String {
    fn visit(node: Node, next: &mut u32, visited: &mut BTreeMap<NodeUID, Vec<u32>>) -> String {
        let mut out = String::new();
        let ins = node.ports_in();
        for inp in &ins {
            out += visit(inp.get_node(), next, visited).as_str();
        }
        let outs = node.ports_out();
        let out_ids: Vec<_> = outs
            .iter()
            .map(|_| {
                let v = *next;
                *next += 1;
                v
            })
            .collect();
        out += &out_ids.iter().map(|x| format!("%{x}")).join(", ");
        out += " = ";
        out += node.get_type().dialect.get_name();
        out += ".";
        out += node.get_type().get_name();
        if ins.len() > 0 {
            out += " ";
            out += &ins
                .iter()
                .map(|x| {
                    let id = visited.get(&x.get_node().get_uid()).unwrap()[x.get_index()];
                    format!("%{id}")
                })
                .join(", ");
        };
        out += ";\n";

        visited.insert(node.get_uid(), out_ids);

        out
    }

    let mut visited = BTreeMap::<NodeUID, Vec<u32>>::new();
    let mut next = 0;
    visit(node, &mut next, &mut visited)
}
