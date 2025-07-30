use crate::*;
use std::any::Any;
use std::borrow::Cow;
use std::hash::Hash;

#[derive(Eq, PartialEq, Hash)]
pub struct TypeVarImpl {
    pub(crate) dialect: DialectRef,
    pub(crate) name: String,
    pub(crate) ground_args: Vec<String>,
}

impl std::fmt::Display for TypeVarImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.dialect.get_name(), self.get_name())
    }
}

impl std::fmt::Debug for TypeVarImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct GroundArgRef {
    to: TypeVar,
    idx: usize,
}

impl TypeVarImpl {
    pub fn get_dialect(&self) -> DialectRef {
        self.dialect.clone()
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ground_arg(self: &Arc<Self>, name: &str) -> Result<GroundArgRef, TypeError> {
        self.ground_args
            .iter()
            .find_position(|x| x.as_str() == name)
            .ok_or_else(|| TypeError::GroundArgNotRegistered {
                tag: self.clone(),
                name: name.to_string(),
            })
            .map(|(idx, _)| GroundArgRef {
                to: self.clone(),
                idx,
            })
    }
}

pub type TypeVar = Arc<TypeVarImpl>;

#[cfg(feature = "quote")]
impl quote::ToTokens for TypeVarImpl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::quote;

        let dia = self.get_dialect();
        let dia = dia.as_ref();
        let ty = self.get_name();
        tokens.extend(quote! { #dia.find_type(#ty).unwrap() });
    }
}

pub trait TryQuote {
    #[cfg(feature = "quote")]
    fn try_quote(&self) -> Result<proc_macro2::TokenStream, ()> {
        Err(())
    }
}

#[cfg(feature = "quote")]
impl<T: quote::ToTokens> TryQuote for T {
    fn try_quote(&self) -> Result<proc_macro2::TokenStream, ()> {
        let mut v = proc_macro2::TokenStream::new();
        self.to_tokens(&mut v);
        Ok(v)
    }
}

pub trait CustomTypeError: std::fmt::Display + std::fmt::Debug {}

/// the Eq trait does not need to match, and instead only checks strict equaity.
pub trait CustomTypeInner: std::fmt::Display + Any + Send + Sync + TryQuote {
    fn as_any(&self) -> &dyn Any;

    /// only ever gets called when the other side is the same impl
    fn unify(
        &self,
        self_repr: &CustomType,
        other: &CustomType,
    ) -> Result<Type, Box<dyn CustomTypeError>>;

    /// check if other is your type and then also check if this matches
    fn matches(&self, self_repr: &CustomType, other: &CustomType) -> bool;

    fn hash(&self, self_repr: &CustomType) -> u64;
}

pub trait SimpleCustomType: Sized + Hash + std::fmt::Display + Send + Sync + TryQuote {
    type Error: Sized + CustomTypeError;

    fn unify(&self, other: &Self) -> Result<Self, Self::Error>;
    fn matches(&self, other: &Self) -> bool;
}

impl<T: 'static + SimpleCustomType> CustomTypeInner for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn unify(
        &self,
        self_repr: &CustomType,
        other: &CustomType,
    ) -> Result<Type, Box<dyn CustomTypeError>> {
        self.unify(other.try_cast_to().unwrap())
            .map_err(|x| Box::new(x) as Box<dyn CustomTypeError>)
            .map(|x| Type::custom(CustomType::new(self_repr.get_name(), x)))
    }

    fn matches(&self, _self_repr: &CustomType, other: &CustomType) -> bool {
        self.matches(other.try_cast_to().unwrap())
    }

    fn hash(&self, _self_repr: &CustomType) -> u64 {
        use std::hash::*;
        let mut h = DefaultHasher::new();
        self.hash(&mut h);
        h.finish()
    }
}

pub struct CustomType {
    var: TypeVar,
    inner: Box<dyn CustomTypeInner>,
}

#[cfg(feature = "quote")]
impl TryQuote for CustomType {
    fn try_quote(&self) -> Result<proc_macro2::TokenStream, ()> {
        let inner = self.get_inner().try_quote()?;
        let ty = self.get_name();
        let ty = ty.as_ref();
        Ok(quote::quote! { vxcc_ir::types::CustomType::new(#ty, #inner) })
    }
}

impl PartialEq for CustomType {
    fn eq(&self, other: &Self) -> bool {
        self.matches(other)
    }
}

impl Eq for CustomType {}

impl Hash for CustomType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        state.write_u64(self.inner.hash(self));
    }
}

impl CustomType {
    pub fn new<T: CustomTypeInner>(var: TypeVar, val: T) -> Self {
        CustomType {
            var,
            inner: Box::new(val),
        }
    }

    pub fn get_name(&self) -> TypeVar {
        self.var.clone()
    }

    fn get_inner(&self) -> &dyn CustomTypeInner {
        self.inner.as_ref()
    }

    pub fn try_cast_to<T: CustomTypeInner>(&self) -> Option<&T> {
        self.get_inner().as_any().downcast_ref()
    }

    fn unify(&self, other: &Self) -> Result<Type, TypeError> {
        self.get_inner()
            .unify(self, other)
            .map_err(|x| CustomTypeErrorWrapper::new(x, self.get_name()).into())
    }

    fn matches(&self, other: &Self) -> bool {
        self.get_name() == other.get_name() && self.get_inner().matches(self, other)
    }
}

impl std::fmt::Display for CustomType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.get_name(), self.get_inner())
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct TypeAnd {
    conjugate: Vec<Type>,
}

impl std::fmt::Display for TypeAnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut any = false;
        for (idx, x) in self.get_all().enumerate() {
            any = true;
            if idx > 0 {
                write!(f, " + ")?;
            }

            // need parenthesis arround inner TAnd (which shouldn't even exist!)
            match *x.0 {
                TypeImpl::Any
                | TypeImpl::Var(_)
                | TypeImpl::Unspec(_)
                | TypeImpl::Custom(_)
                | TypeImpl::Ground(_) => write!(f, "{}", x)?,

                TypeImpl::And(_) => write!(f, "({})", x)?,
            }
        }
        if !any {
            write!(f, "()")?;
        }
        Ok(())
    }
}

impl TypeAnd {
    /// this allows nested ands and flattens them
    ///
    /// this also deduplicates
    fn try_from<I: Iterator<Item = Type>>(value: I) -> Result<Self, TypeError> {
        let mut conjugate = Vec::<Type>::new();

        let mut add = |item: Type| -> Result<(), TypeError> {
            match &*item.0 {
                TypeImpl::Any => {}

                TypeImpl::Unspec(_) => {
                    if !conjugate.contains(&item) {
                        conjugate.push(item);
                    }
                }

                TypeImpl::Var(v) => {
                    if !conjugate.iter().any(|x| match &*x.0 {
                        TypeImpl::Var(x) => *x == *v,
                        _ => false,
                    }) {
                        conjugate.push(item);
                    }
                }

                TypeImpl::Custom(custom) => {
                    if let Some((idx, other)) = conjugate.iter().find_position(|x| match &*x.0 {
                        TypeImpl::Custom(c) => c.get_name() == custom.get_name(),
                        _ => false,
                    }) {
                        let other = match &*other.0 {
                            TypeImpl::Custom(c) => c,
                            _ => unreachable!(),
                        };
                        conjugate[idx] = custom.unify(other)?;
                    } else {
                        conjugate.push(item);
                    }
                }

                TypeImpl::Ground(g) => {
                    let ptr = conjugate
                        .iter()
                        .map(|x| match &*x.0 {
                            TypeImpl::Ground(g) => Some(g),
                            _ => None,
                        })
                        .find_position(|xg| xg.is_some_and(|xg| xg.tag == g.tag));

                    if let Some((idx, xg)) = ptr {
                        let xg = xg.unwrap();

                        let newv = g
                            .inner
                            .0
                            .iter()
                            .zip(xg.inner.0.iter())
                            .map(|(a, b)| a.and_pair(b))
                            .collect::<Result<_, _>>()?;

                        conjugate[idx] = Type::from(TypeImpl::Ground(TypeGround {
                            tag: g.get_tag(),
                            inner: TypeGroundInner(newv),
                        }))
                    } else {
                        conjugate.push(item);
                    }
                }

                TypeImpl::And(_) => unreachable!(),
            };

            Ok(())
        };

        for item in value {
            match &*item.0 {
                TypeImpl::And(nested) => {
                    // recursively flattens
                    let nested = Self::try_from(nested.get_all())?.conjugate;
                    for x in nested.into_iter() {
                        add(x)?;
                    }
                }

                _ => {
                    add(item)?;
                }
            }
        }

        Ok(TypeAnd { conjugate })
    }

    pub fn get_all(&self) -> impl Iterator<Item = Type> {
        self.conjugate.iter().map(|x| x.clone())
    }
}

#[derive(Eq, PartialEq, Hash)]
struct TypeGroundInner(Vec<Type>);

#[derive(Eq, PartialEq, Hash)]
pub struct TypeGround {
    tag: TypeVar,
    inner: TypeGroundInner,
}

impl std::ops::Index<GroundArgRef> for TypeGround {
    type Output = Type;

    fn index(&self, index: GroundArgRef) -> &Self::Output {
        assert_eq!(index.to, self.get_tag());
        &self.inner.0[index.idx]
    }
}

impl TypeGround {
    pub fn get_tag(&self) -> TypeVar {
        self.tag.clone()
    }

    pub fn get_params(&self) -> Vec<(String, Type)> {
        self.get_tag()
            .ground_args
            .iter()
            .zip(self.inner.0.iter())
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect()
    }
}

impl std::fmt::Display for TypeGround {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.tag)?;
        for (idx, (key, val)) in self.get_params().into_iter().enumerate() {
            if idx > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, val)?;
        }
        write!(f, "}}")
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct TypeNumList {
    elts: Vec<i64>,
}

impl TypeNumList {
    pub fn len(&self) -> usize {
        self.elts.len()
    }

    pub fn get_elements(&self) -> impl Iterator<Item = i64> {
        self.elts.iter().map(|x| *x)
    }
}

impl std::fmt::Display for TypeNumList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.elts)
    }
}

/// you shouldn't manually create these. Use the constructors from Type
#[derive(Eq, PartialEq, Hash)]
#[non_exhaustive]
pub enum TypeImpl {
    Any,
    Var(TypeVar),

    /// nested TypeImpl::And is not allowed!
    And(TypeAnd),

    Ground(TypeGround),

    Custom(CustomType),

    Unspec(String),
}

impl std::fmt::Display for TypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeImpl::Any => write!(f, "?"),
            TypeImpl::Var(inner) => write!(f, "{}", inner),
            TypeImpl::And(inner) => write!(f, "{}", inner),
            TypeImpl::Ground(inner) => write!(f, "{}", inner),
            TypeImpl::Custom(inner) => write!(f, "{}", inner),
            TypeImpl::Unspec(inner) => write!(f, "?{}", inner),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash)]
pub struct Type(pub Arc<TypeImpl>);

#[cfg(feature = "quote")]
impl TryQuote for Type {
    fn try_quote(&self) -> Result<proc_macro2::TokenStream, ()> {
        use quote::quote;

        Ok(match &*self.0 {
            TypeImpl::Any => quote! { vxcc_ir::types::Type::any() },

            TypeImpl::Var(type_var_impl) => {
                let name = type_var_impl.as_ref();
                quote! { vxcc_ir::types::Type::var(#name) }
            }

            TypeImpl::And(inner) => {
                let inner = inner
                    .get_all()
                    .map(|x| x.try_quote().map(|x| quote! { #x, }))
                    .collect::<Result<proc_macro2::TokenStream, _>>()?;

                quote! { vxcc_ir::types::Type::and([#inner].into_iter()) }
            }

            TypeImpl::Ground(g) => {
                let name = g.get_tag();
                let name = name.as_ref();

                let keys = g
                    .get_params()
                    .into_iter()
                    .map(|(l, r)| r.try_quote().map(|r| quote! { (#l,#r), }))
                    .collect::<Result<proc_macro2::TokenStream, _>>()?;

                quote! { vxcc_ir::types::Type::ground_kv(#name, [#keys].into_iter()) }
            }

            TypeImpl::Custom(custom) => {
                let custom = custom.try_quote()?;
                quote! { vxcc_ir::types::Type::custom(#custom) }
            }

            TypeImpl::Unspec(v) => quote! { vxcc_ir::types::Type::unspec(#v) },
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}`", self)
    }
}

impl From<TypeImpl> for Type {
    fn from(value: TypeImpl) -> Self {
        Self(Arc::new(value))
    }
}

static DENORM_LUT: LazyLock<Mutex<HashMap<TypeVar, Vec<(Type, Type)>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Debug)]
pub struct CustomTypeErrorWrapper {
    tyname: TypeVar,
    err: Box<dyn CustomTypeError>,
}

impl std::error::Error for CustomTypeErrorWrapper {}

impl CustomTypeErrorWrapper {
    fn new(inner: Box<dyn CustomTypeError>, tyname: TypeVar) -> Self {
        CustomTypeErrorWrapper { tyname, err: inner }
    }
}

impl std::fmt::Display for CustomTypeErrorWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error while handeling {}: {}", self.tyname, self.err)
    }
}

#[derive(Debug)]
pub enum TypeError {
    NotAllTemplatesExpanded,

    LeftHandSideUnspecNotAllowed,

    GroundArgNotRegistered { tag: TypeVar, name: String },

    GroundArgNotSet { tag: TypeVar, name: String },

    Custom(CustomTypeErrorWrapper),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::NotAllTemplatesExpanded => write!(
                f,
                "not all templates could be expanded in implies expression"
            ),
            TypeError::LeftHandSideUnspecNotAllowed => write!(
                f,
                "type templates are only allowed on the right hand side (in implies)"
            ),
            TypeError::GroundArgNotRegistered { tag, name } => {
                write!(f, "ground argument {name} not a member of {tag}")
            }
            TypeError::GroundArgNotSet { tag, name } => write!(
                f,
                "{tag} expects ground argument {name}, but it was not provided"
            ),
            TypeError::Custom(inner) => write!(f, "{inner}"),
        }
    }
}

impl From<CustomTypeErrorWrapper> for TypeError {
    fn from(value: CustomTypeErrorWrapper) -> Self {
        Self::Custom(value)
    }
}

impl Into<Type> for CustomType {
    fn into(self) -> Type {
        Type::custom(self)
    }
}

impl Type {
    fn like_and(&self) -> Vec<Type> {
        match &*self.0 {
            TypeImpl::And(g) => g.get_all().collect(),
            _ => vec![self.clone()],
        }
    }

    pub fn custom(c: CustomType) -> Self {
        Self::from(TypeImpl::Custom(c))
    }

    pub fn any() -> Self {
        Self::from(TypeImpl::Any)
    }

    pub fn var(name: &TypeVar) -> Self {
        Self::from(TypeImpl::Var(name.clone()))
    }

    pub fn unspec(name: &str) -> Self {
        Self::from(TypeImpl::Unspec(name.to_string()))
    }

    pub fn and<I: Iterator<Item = Type>>(from: I) -> Result<Self, TypeError> {
        TypeAnd::try_from(from).map(|x| Self::from(TypeImpl::And(x)))
    }

    pub fn and_pair(&self, b: &Self) -> Result<Type, TypeError> {
        Self::and([self.clone(), b.clone()].into_iter())
    }

    /// can be used for named type arguments, and structs
    ///
    /// if there are no arguments, this panics!
    pub fn ground_kv<'a, I: Iterator<Item = (GroundArgRef, Type)>>(
        tag: &TypeVar,
        from: I,
    ) -> Result<Self, TypeError> {
        let mut out = Vec::<Option<Type>>::new();
        for _ in 0..tag.ground_args.len() {
            out.push(None);
        }

        for (k, v) in from {
            assert_eq!(&k.to, tag);
            if out[k.idx].is_some() {
                Err(TypeError::GroundArgNotSet {
                    tag: tag.clone(),
                    name: k.to.ground_args[k.idx].clone(),
                })?
            }
            out[k.idx] = Some(v);
        }

        let out = out
            .into_iter()
            .enumerate()
            .map(|(idx, x)| {
                x.ok_or_else(|| TypeError::GroundArgNotSet {
                    tag: tag.clone(),
                    name: tag.ground_args[idx].clone(),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self::from(TypeImpl::Ground(TypeGround {
            tag: tag.clone(),
            inner: TypeGroundInner(out),
        })))
    }

    fn optimize(&self) -> Self {
        match &*self.0 {
            TypeImpl::And(a) => {
                if a.conjugate.len() == 1 {
                    a.conjugate[0].clone()
                } else {
                    self.clone()
                }
            }

            _ => self.clone(),
        }
    }

    pub fn unify(&self, other: &Self) -> Result<Self, TypeError> {
        Self::and_pair(self, other).map(|x| x.optimize())
    }

    pub(crate) fn denorm_add(
        /* from TVar or TGround */ ty: TypeVar,
        requires: Type,
        implies: Type,
    ) {
        DENORM_LUT
            .lock()
            .unwrap()
            .entry(ty)
            .or_insert(Vec::new())
            .push((requires, implies));
    }

    fn denorm_options(&self) -> Vec<(Type, Type)> {
        drop(DialectRegistry::get()); // to call lazyinit funcs

        match &*self.0 {
            TypeImpl::Var(v) => DENORM_LUT
                .lock()
                .unwrap()
                .get(v)
                .cloned()
                .unwrap_or(Vec::new()),

            TypeImpl::Ground(g) => DENORM_LUT
                .lock()
                .unwrap()
                .get(&g.get_tag())
                .cloned()
                .unwrap_or(Vec::new()),

            TypeImpl::And(a) => a
                .get_all()
                .flat_map(|x| x.denorm_options().into_iter())
                .collect(),

            _ => vec![],
        }
    }

    fn denormx(&self) -> Result<(Self, bool), TypeError> {
        match &*self.0 {
            TypeImpl::Unspec(_) => Err(TypeError::LeftHandSideUnspecNotAllowed)?,
            _ => {}
        }

        let mut out = self.clone();

        let mut anychanged = false;

        loop {
            let mut changed = false;

            let mut new_cases = Vec::new();
            for case in out.like_and().into_iter() {
                let mut out = case.clone();
                match &*case.0 {
                    TypeImpl::Any
                    | TypeImpl::Var(_)
                    | TypeImpl::Unspec(_)
                    | TypeImpl::Custom(_) => (),

                    TypeImpl::And(_) => unreachable!(),

                    TypeImpl::Ground(g) => {
                        let mut schg = false;
                        let new = g
                            .inner
                            .0
                            .iter()
                            .map(|v| {
                                v.denormx().map(|(v, c)| {
                                    schg = c;
                                    v
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        if schg {
                            out = Type::from(TypeImpl::Ground(TypeGround {
                                tag: g.get_tag(),
                                inner: TypeGroundInner(new),
                            }));
                            changed = true;
                        }
                    }
                }
                new_cases.push(out);
            }
            out = Type::and(new_cases.into_iter())?;

            for (req, implies) in self.denorm_options() {
                let mut map = HashMap::new();
                if self.fast_matches(&req, &mut map) {
                    let old_out = out;
                    let implied = implies.expand_unspec(&map)?;
                    out = Type::and_pair(&old_out, &implied)?;

                    let mut thefuckisthis = HashMap::new();
                    if !old_out.fast_matches(&out, &mut thefuckisthis) {
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
            anychanged = true;
        }

        Ok((out, anychanged))
    }

    /// applies all "implies" statements from all dialects to all inner types.
    pub fn denorm(&self) -> Result<Self, TypeError> {
        self.denormx().map(|x| x.0)
    }

    pub fn expand_unspec(self, map: &HashMap<Cow<str>, Type>) -> Result<Type, TypeError> {
        match &*self.0 {
            TypeImpl::Unspec(v) => Ok(map
                .get(v.as_str())
                .ok_or(TypeError::NotAllTemplatesExpanded)?
                .clone()
                .expand_unspec(map)?),

            TypeImpl::And(a) => Type::and(
                a.get_all()
                    .map(|x| x.expand_unspec(map))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter(),
            ),

            TypeImpl::Ground(g) => Ok(Type::from(TypeImpl::Ground(TypeGround {
                tag: g.get_tag(),
                inner: TypeGroundInner(
                    g.inner
                        .0
                        .iter()
                        .map(|v| v.clone().expand_unspec(map))
                        .collect::<Result<Vec<_>, _>>()?,
                ),
            }))),

            _ => Ok(self),
        }
    }

    pub(crate) fn to_var_list(&self) -> Vec<TypeVar> {
        match &*self.0 {
            TypeImpl::Any => vec![],
            TypeImpl::Var(v) => vec![v.clone()],
            TypeImpl::Custom(_) => vec![],
            TypeImpl::Unspec(_) => vec![],
            TypeImpl::Ground(g) => vec![g.get_tag()],
            TypeImpl::And(a) => a
                .get_all()
                .flat_map(|x| x.to_var_list().into_iter())
                .collect(),
        }
    }

    pub fn approx_expressiveness(&self) -> usize {
        match &*self.0 {
            TypeImpl::Any => 0,
            TypeImpl::Var(_) => 1,
            TypeImpl::Custom(_) => 2,
            TypeImpl::Unspec(_) => 2,
            TypeImpl::And(and) => and.get_all().map(|x| x.approx_expressiveness()).sum(),
            TypeImpl::Ground(_) => 5,
        }
    }

    /// does this type have all types from the other type?
    ///
    /// this calls [Self::denorm] first
    pub fn matches(&self, other: &Self) -> Result<bool, TypeError> {
        if self == other {
            return Ok(true);
        }

        let mut map = HashMap::new();
        Ok(self.denorm()?.fast_matches(other, &mut map))
    }

    fn fast_matches(&self, other: &Self, out: &mut HashMap<Cow<str>, Type>) -> bool {
        fn match_ground(
            this: &TypeGround,
            other: &TypeGround,
            out: &mut HashMap<Cow<str>, Type>,
        ) -> bool {
            if this.tag != other.tag {
                return false;
            }

            this.inner
                .0
                .iter()
                .zip(other.inner.0.iter())
                .all(|(t, o)| t.fast_matches(o, out))
        }

        match &*other.0 {
            TypeImpl::Any => true,

            TypeImpl::Unspec(u) => {
                out.insert(u.clone().into(), self.clone());
                true
            }

            TypeImpl::Var(var) => match &*self.0 {
                TypeImpl::Var(x) => *var == *x,
                TypeImpl::And(x) => x.get_all().any(|x| x.fast_matches(other, out)),
                _ => false,
            },

            TypeImpl::And(and) => {
                if and.get_all().all(|x| match &*x.0 {
                    TypeImpl::Unspec(_) => true,
                    _ => self.fast_matches(&x, out),
                }) {
                    for x in and.get_all() {
                        match &*x.0 {
                            TypeImpl::Unspec(u) => {
                                out.insert(u.clone().into(), self.clone());
                            }

                            _ => {}
                        }
                    }
                    true
                } else {
                    false
                }
            }

            TypeImpl::Ground(ground) => self.like_and().into_iter().any(|item| match &*item.0 {
                TypeImpl::Ground(x) => match_ground(x, ground, out),

                _ => false,
            }),

            TypeImpl::Custom(custom) => match &*self.0 {
                TypeImpl::Custom(x) => custom.matches(x),
                TypeImpl::And(x) => x.get_all().any(|x| x.fast_matches(other, out)),
                _ => false,
            },
        }
    }
}
