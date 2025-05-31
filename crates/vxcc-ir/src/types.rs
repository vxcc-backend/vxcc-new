use std::borrow::Cow;

use crate::*;

#[derive(Eq, PartialEq, Hash)]
pub struct TypeVarImpl {
    pub(crate) dialect: DialectRef,
    pub(crate) name: String,
}

impl std::fmt::Display for TypeVarImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.dialect.get_name(), self.get_name())
    }
}

impl TypeVarImpl {
    pub fn get_dialect(&self) -> DialectRef {
        self.dialect.clone()
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }
}

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

pub type TypeVar = Arc<TypeVarImpl>;

#[derive(Eq, PartialEq, Hash)]
pub struct TypeAnd {
    conjugate: Vec<Type>
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
                TypeImpl::Any        |
                TypeImpl::Var(_)     |
                TypeImpl::Unspec(_)  |
                TypeImpl::NumList(_) |
                TypeImpl::Ground(_) => write!(f, "{}", x)?,

                TypeImpl::And(_) => write!(f, "({})", x)?,
            }
        }
        if !any {
            write!(f, "()")?;
        }
        Ok(())
    }
}

impl<I: Iterator<Item = Type>> From<I> for TypeAnd {
    /// this allows nested ands and flattens them
    ///
    /// this also deduplicates
    fn from(value: I) -> Self {
        let mut conjugate = Vec::<Type>::new();

        let mut add = |item: Type| {
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
                        _ => false })
                    {
                        conjugate.push(item);
                    }
                }

                TypeImpl::NumList(_) => {
                    conjugate.push(item);
                }

                TypeImpl::Ground(g) => {
                    let ptr = conjugate.iter()
                        .map(|x| match &*x.0 {
                            TypeImpl::Ground(g) => Some(g),
                            _ => None,
                        })
                        .find_position(|xg| xg.is_some_and(|xg| xg.tag == g.tag));

                    if let Some((idx, xg)) = ptr {
                        let xg = xg.unwrap();

                        let tag = &g.tag;
                        let g = g.get_params();
                        let xg = xg.get_params();

                        conjugate[idx] = Type::ground_kv(tag,
                            g.keys().chain(xg.keys()).dedup()
                            .map(|key| {
                                let dedup = [g.get(key), xg.get(key)].into_iter()
                                        .filter_map(|x| x.cloned())
                                        .collect::<Vec<_>>();
                                (key.as_str(), Type::and(dedup.into_iter()))
                            }));
                    } else {
                        conjugate.push(item);
                    }
                }

                TypeImpl::And(_) => unreachable!(),
            };
        };

        for item in value {
            match &*item.0 {
                TypeImpl::And(nested) => {
                    // recursively flattens
                    let nested = Self::from(nested.get_all()).conjugate;
                    for x in nested.into_iter() {
                        add(x);
                    }
                }

                _ => {
                    add(item);
                }
            }
        }

        TypeAnd { conjugate }
    }
}

impl TypeAnd {
    pub fn get_all(&self) -> impl Iterator<Item = Type> {
        self.conjugate.iter()
            .map(|x| x.clone())
    }
}

#[derive(Eq, PartialEq)]
enum TypeGroundInner {
    KV(HashMap<String, Type>),
    List(Vec<Type>),
}

impl std::hash::Hash for TypeGroundInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeGroundInner::KV(kv) => {
                for (k,v) in kv.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }

            TypeGroundInner::List(li) => {
                li.hash(state);
            }
        }
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct TypeGround {
    tag: TypeVar,
    inner: TypeGroundInner,
}

impl TypeGround {
    pub fn get_tag(&self) -> TypeVar {
        self.tag.clone()
    }

    pub fn get_param(&self, name: &str) -> Option<Type> {
        match &self.inner {
            TypeGroundInner::KV(kv) => kv.get(name).cloned(),
            TypeGroundInner::List(li) => name.parse().ok().and_then(|x: usize| li.get(x)).cloned()
        }
    }

    pub fn get_params(&self) -> HashMap<String, Type> {
        match &self.inner {
            TypeGroundInner::KV(kv) => kv.iter()
                .map(|(k,v)| (k.clone(), v.clone()))
                .collect(),

            TypeGroundInner::List(li) => li.iter()
                .enumerate()
                .map(|(k,v)| (k.to_string(), v.clone()))
                .collect()
        }
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

    NumList(TypeNumList),

    Unspec(String),
}

impl std::fmt::Display for TypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeImpl::Any => write!(f, "?"),
            TypeImpl::Var(inner) => write!(f, "{}", inner),
            TypeImpl::And(inner) => write!(f, "{}", inner),
            TypeImpl::Ground(inner) => write!(f, "{}", inner),
            TypeImpl::NumList(inner) => write!(f, "{}", inner),
            TypeImpl::Unspec(inner) => write!(f, "?{}", inner),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash)]
pub struct Type(pub Arc<TypeImpl>);

#[cfg(feature = "quote")]
impl quote::ToTokens for Type {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        use quote::quote;

        tokens.extend(match &*self.0 {
            TypeImpl::Any => quote! { vxcc_ir::types::Type::any() },

            TypeImpl::Var(type_var_impl) => {
                let name = type_var_impl.as_ref();
                quote! { vxcc_ir::types::Type::var(#name) }
            }

            TypeImpl::And(inner) => {
                let inner = inner.get_all()
                    .flat_map(|x| quote! { #x, })
                    .collect::<proc_macro2::TokenStream>();

                quote! { vxcc_ir::types::Type::and([#inner].into_iter()) }
            }

            TypeImpl::Ground(g) => {
                let name = g.get_tag();
                let name = name.as_ref();

                let keys = g.get_params()
                    .into_iter()
                    .flat_map(|(l,r)| quote! { (#l,#r), })
                    .collect::<proc_macro2::TokenStream>();

                quote! { vxcc_ir::types::Type::ground_kv(#name, [#keys].into_iter()) }
            }

            TypeImpl::NumList(type_num_list) => {
                let inner = type_num_list.get_elements()
                    .flat_map(|x| quote! { #x , })
                    .collect::<proc_macro2::TokenStream>();
                quote! { vxcc_ir::types::Type::num_list([#inner].into_iter()) }
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

pub trait TypeUnify {
    fn unify(&self, other: &Type) -> Type;
}

impl TypeUnify for Option<&Type> {
    fn unify(&self, other: &Type) -> Type {
        if let Some(x) = self {
            x.unify(other)
        } else {
            other.clone()
        }
    }
}

impl TypeUnify for Option<Type> {
    fn unify(&self, other: &Type) -> Type {
        if let Some(x) = self {
            x.unify(other)
        } else {
            other.clone()
        }
    }
}

lazy_static::lazy_static! {
    static ref DENORM_LUT: Mutex<HashMap<TypeVar, Vec<(Type, Type)>>> = Mutex::new(HashMap::new());
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("not all templates could be expanded in implies expression")]
    NotAllTemplatesExpanded,

    #[error("type templates are only allowed on the right hand side (in implies)")]
    LeftHandSideUnspecNotAllowed
}

impl Type {
    fn like_and(&self) -> Vec<Type> {
        match &*self.0 {
            TypeImpl::And(g) => g.get_all().collect(),
            _ => vec!(self.clone())
        }
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

    pub fn and<I: Iterator<Item = Type>>(from: I) -> Self {
        Self::from(TypeImpl::And(TypeAnd::from(from)))
    }

    pub fn and_pair(&self, b: &Self) -> Self {
        Self::and([self.clone(), b.clone()].into_iter())
    }

    // TODO: instead require type arguments to be registered!

    /// can be used for named type arguments, and structs
    ///
    /// if there are no arguments, this panics!
    pub fn ground_kv<'a, S: AsRef<str>, I: Iterator<Item = (S, Type)>>(tag: &TypeVar, from: I) -> Self {
        let inner: HashMap<_,_> = from
            .map(|(k,v)| (k.as_ref().to_string(), v))
            .collect();

        if inner.is_empty() {
            panic!("ground args are empty");
        } else {
            Self::from(TypeImpl::Ground(TypeGround {
                tag: tag.clone(),
                inner: TypeGroundInner::KV(inner)
            }))
        }
    }

    /// like [Self::ground_kv], but enumerates names with 0..
    ///
    /// should be used for things like tuples
    ///
    /// if there are no arguments, this simplifies to [Self::var]
    pub fn ground_nums<I: Iterator<Item = Type>>(tag: &TypeVar, from: I) -> Self {
        let inner: Vec<_> = from.collect();

        if inner.is_empty() {
            Self::var(tag)
        } else {
            Self::from(TypeImpl::Ground(TypeGround {
                tag: tag.clone(),
                inner: TypeGroundInner::List(inner)
            }))
        }
    }

    pub fn num_list<I: Iterator<Item = i64>>(from: I) -> Self {
        Self::from(TypeImpl::NumList(TypeNumList { elts: from.collect() }))
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

            _ => self.clone()
        }
    }

    pub fn unify(&self, other: &Self) -> Self {
        let tand = Self::and_pair(self, other);
        tand.optimize()
    }

    pub(crate) fn denorm_add( /* from TVar or TGround */ ty: TypeVar, requires: Type, implies: Type) {
        DENORM_LUT.lock().unwrap()
            .entry(ty)
            .or_insert(Vec::new())
            .push((requires, implies));
    }

    fn denorm_options(&self) -> Vec<(Type, Type)> {
        match &*self.0 {
            TypeImpl::Var(v) => {
                DENORM_LUT.lock().unwrap()
                    .get(v)
                    .cloned()
                    .unwrap_or(Vec::new())
            }

            TypeImpl::Ground(g) => {
                DENORM_LUT.lock().unwrap()
                    .get(&g.get_tag())
                    .cloned()
                    .unwrap_or(Vec::new())
            }

            TypeImpl::And(a) => {
                a.get_all()
                    .flat_map(|x| x.denorm_options().into_iter())
                    .collect()
            }

            _ => vec!()
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
                    TypeImpl::Any |
                    TypeImpl::Var(_) |
                    TypeImpl::Unspec(_) |
                    TypeImpl::NumList(_) => (),

                    TypeImpl::And(_) => panic!(),

                    TypeImpl::Ground(g) => {
                        let mut new: Option<Vec<_>> = None;
                        for (idx, item) in g.get_params().into_iter().enumerate() {
                            let (anew, ch) = item.1.denormx()?;
                            if ch || new.is_some() {
                                if let Some(new) = &mut new {
                                    new[idx] = (item.0, anew);
                                } else {
                                    let mut v = g.get_params().into_iter().collect::<Vec<_>>();
                                    v[idx] = (item.0, anew);
                                    new = Some(v);
                                }
                            }
                        }

                        if let Some(new) = new {
                            out = Type::ground_kv(&g.get_tag(), new.into_iter());
                            changed = true;
                        }
                    }
                }
                new_cases.push(out);
            }
            out = Type::and(new_cases.into_iter());

            for (req, implies) in self.denorm_options() {
                let mut map = HashMap::new();
                if self.fast_matches(&req, &mut map) {
                    let old_out = out;
                    println!("{:#?}", map);
                    let implied = implies.expand_unspec(&map)?;
                    println!("{old_out} => {implied}");
                    out = Type::and_pair(&old_out, &implied);

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
            TypeImpl::Unspec(v) =>
                Ok(map.get(v.as_str())
                    .ok_or(TypeError::NotAllTemplatesExpanded)?
                    .clone()
                    .expand_unspec(map)?),

            TypeImpl::And(a) =>
                Ok(Type::and(a.get_all()
                        .map(|x| x.expand_unspec(map))
                        .collect::<Result<Vec<_>,_>>()?
                        .into_iter())),

            TypeImpl::Ground(g) =>
                Ok(Type::ground_kv(&g.get_tag(), g.get_params()
                        .iter()
                        .map(|(k,v)| v.clone().expand_unspec(map).map(|x| (k.as_str(), x)))
                        .collect::<Result<Vec<_>,_>>()?
                        .into_iter())),

            _ => Ok(self)
        }
    }

    pub(crate) fn to_var_list(&self) -> Vec<TypeVar> {
        match &*self.0 {
            TypeImpl::Any => vec!(),
            TypeImpl::Var(v) => vec!(v.clone()),
            TypeImpl::NumList(_) => vec!(),
            TypeImpl::Unspec(_) => vec!(),
            TypeImpl::Ground(g) => vec!(g.get_tag()),
            TypeImpl::And(a) => a.get_all().flat_map(|x| x.to_var_list().into_iter()).collect()
        }
    }

    pub fn approx_expressiveness(&self) -> usize {
        match &*self.0 {
            TypeImpl::Any => 0,
            TypeImpl::Var(_) => 1,
            TypeImpl::NumList(_) => 2,
            TypeImpl::Unspec(_) => 2,
            TypeImpl::And(and) => and.get_all().map(|x| x.approx_expressiveness()).sum(),
            TypeImpl::Ground(_) => 5,
        }
    }

    /// does this type have all types from the other type?
    ///
    /// this calls [Self::denorm] first
    pub fn matches(&self, other: &Self) -> Result<bool, TypeError> {
        let mut map = HashMap::new();
        Ok(self.denorm()?.fast_matches(other, &mut map))
    }

    fn fast_matches(&self, other: &Self, out: &mut HashMap<Cow<str>, Type>) -> bool {
        fn match_ground(this: &TypeGround, other: &TypeGround, out: &mut HashMap<Cow<str>, Type>) -> bool {
            if this.tag != other.tag {
                return false;
            }

            for (k,v) in other.get_params().into_iter() {
                let tv = this.get_param(k.as_str());
                if let Some(tv) = tv {
                    println!("  {tv} onto {v}");
                    if !tv.fast_matches(&v, out) {
                        return false;
                    }
                } else {
                    return false;
                }
            }

            true
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
                _ => false
            }

            TypeImpl::And(and) => {
                if and.get_all().all(|x| match &*x.0 { TypeImpl::Unspec(_) => true, _ => self.fast_matches(&x, out) }) {
                    for x in and.get_all() {
                        match &*x.0 {
                            TypeImpl::Unspec(u) => {
                                out.insert(u.clone().into(), self.clone());
                            }

                            _ => {}
                        }
                    }
                    true
                }
                else {
                    false
                }
            }

            TypeImpl::Ground(ground) => {
                self.like_and()
                    .into_iter()
                    .any(|item| match &*item.0 {
                        TypeImpl::Ground(x) => {
                            match_ground(x, ground, out)
                        }

                        _ => false,
                    })
            }

            TypeImpl::NumList(numlist) => match &*self.0 {
                TypeImpl::NumList(th) => th == numlist,
                _ => false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_unify() {
        let mut test_dialect = DialectBuilder::new("test");
        let ty_i8 = Type::var(&test_dialect.add_type("I8"));
        let ty_num = Type::var(&test_dialect.add_type("Number"));
        let ty_uint = Type::var(&test_dialect.add_type("UInt"));
        let _test_dialect = test_dialect.build().get_dialect();

        let uni = Type::and_pair(&ty_i8, &ty_num).unify(&ty_uint);
        assert!(uni.matches(&Type::and_pair(&Type::and_pair(&ty_i8, &ty_uint), &ty_num)).unwrap());
        assert!(uni.matches(&ty_i8).unwrap());
        assert!(uni.matches(&ty_num).unwrap());
        assert!(uni.matches(&ty_uint).unwrap());
        assert!(uni.matches(&Type::and_pair(&ty_uint, &ty_num)).unwrap());
        assert!(!uni.matches(&Type::var(&core_dialect::DIALECT.clone)).unwrap());
        assert!(!ty_i8.matches(&ty_num).unwrap());
    }

    #[test]
    fn test_denorm_and_implies_clone() {
        let mut builder = DialectBuilder::new("test");

        let clone = Type::var(&builder.add_type("Clone"));
        let num = Type::var(&builder.add_type("Num"));
        let u64 = Type::var(&builder.add_type("U64"));

        builder.add_implies(
            u64.clone(),
            clone.clone(),
        ).unwrap();

        let _ = builder.build();

        let orig = Type::and_pair(&num, &u64);
        let denormed = orig.denorm().unwrap();

        assert!(denormed.matches(&clone).unwrap());
        assert!(denormed.matches(&orig).unwrap());
    }

    #[test]
    fn test_denorm_vector_implies_clone() {
        let mut builder = DialectBuilder::new("test");

        let clone = Type::var(&builder.add_type("Clone"));
        let vector = builder.add_type("Vector");
        let u64 = Type::var(&builder.add_type("U64"));

        builder.add_implies(
            Type::ground_kv(&vector, [("elt", Type::any())].into_iter()),
            clone.clone(),
        ).unwrap();

        let _ = builder.build();

        let vec_u64 = Type::ground_kv(&vector, [("elt", u64.clone())].into_iter());
        let denormed = vec_u64.denorm().unwrap();

        assert!(denormed.matches(&clone).unwrap());
        assert!(denormed.matches(&vec_u64).unwrap());
        assert!(!clone.matches(&vec_u64).unwrap()); // Non-symmetric
    }

    #[test]
    fn test_denorm_template_preserves_parameter() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let vector = builder.add_type("Vector");
        let iterable = builder.add_type("Iterable");

        builder.add_implies(
            Type::ground_kv(&vector, [("elt", t.clone())].into_iter()),
            Type::ground_kv(&iterable, [("elt", t.clone())].into_iter()),
        ).unwrap();

        let u64 = Type::var(&builder.add_type("U64"));
        let _ = builder.build();

        let vec_u64 = Type::ground_kv(&vector, [("elt", u64.clone())].into_iter());
        let iter_u64 = Type::ground_kv(&iterable, [("elt", u64.clone())].into_iter());

        let denormed = vec_u64.denorm().unwrap();
        assert!(denormed.matches(&iter_u64).unwrap());
        assert!(denormed.matches(&vec_u64).unwrap());
    }

    #[test]
    fn test_denorm_complex_intersection_rule() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let clone = Type::var(&builder.add_type("Clone"));
        let idfk = Type::var(&builder.add_type("Idfk"));
        let vector = builder.add_type("Vector");
        let iterable = builder.add_type("Iterable");

        builder.add_implies(
            Type::and_pair(
                &clone,
                &Type::ground_kv(&vector, [("elt", Type::and_pair(&t, &clone))].into_iter()),
            ),
            Type::ground_kv(&iterable, [("elt", t.clone())].into_iter()),
        ).unwrap();

        builder.add_implies(
            Type::and_pair(
                &clone,
                &Type::ground_kv(&vector, [("elt", Type::and_pair(&t, &clone))].into_iter()),
            ),
            idfk.clone(),
        ).unwrap();

        let u64 = Type::var(&builder.add_type("U64"));
        let _ = builder.build();

        let input = Type::and_pair(
            &clone,
            &Type::ground_kv(&vector, [("elt", Type::and_pair(&u64, &clone))].into_iter()),
        );

        let iter_u64 = Type::ground_kv(&iterable, [("elt", u64.clone())].into_iter());
        let denormed = input.denorm().unwrap();

        println!("{}", denormed);
        assert!(denormed.matches(&iter_u64).unwrap());
        assert!(denormed.matches(&idfk).unwrap());
        assert!(denormed.matches(&input).unwrap());
    }

    #[test]
    fn test_denorm_wrapper_clone_composite_implies() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let clone = Type::var(&builder.add_type("Clone"));
        let serializable = Type::var(&builder.add_type("Serializable"));
        let debuggable = Type::var(&builder.add_type("Debuggable"));
        let wrapper = builder.add_type("Wrapper");

        let lhs = Type::and_pair(
            &clone,
            &Type::ground_kv(&wrapper, [("inner", Type::and_pair(&t, &clone))].into_iter()),
        );

        builder.add_implies(
            lhs.clone(),
            serializable.clone(),
        ).unwrap();

        builder.add_implies(
            lhs,
            debuggable.clone(),
        ).unwrap();

        let str_ = Type::var(&builder.add_type("Str"));
        let _ = builder.build();

        let input = Type::and_pair(
            &clone,
            &Type::ground_kv(&wrapper, [("inner", Type::and_pair(&str_, &clone))].into_iter()),
        );

        let denormed = input.denorm().unwrap();

        println!("{}", denormed);
        assert!(denormed.matches(&serializable).unwrap());
        assert!(denormed.matches(&debuggable).unwrap());
        assert!(denormed.matches(&input).unwrap());
    }

    #[test]
    fn test_denorm_box_readable_to_decodable() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let readable = Type::var(&builder.add_type("Readable"));
        let decodable = Type::var(&builder.add_type("Decodable"));
        let box_ = builder.add_type("Box");

        builder.add_implies(
            Type::and_pair(
                &readable,
                &Type::ground_kv(&box_, [("val", Type::and_pair(&t, &readable))].into_iter()),
            ),
            decodable.clone(),
        ).unwrap();

        let data = Type::var(&builder.add_type("Data"));
        let _ = builder.build();

        let input = Type::and_pair(
            &readable,
            &Type::ground_kv(&box_, [("val", Type::and_pair(&data, &readable))].into_iter()),
        );

        let denormed = input.denorm().unwrap();

        println!("{}", denormed);
        assert!(denormed.matches(&decodable).unwrap());
        assert!(denormed.matches(&input).unwrap());
    }

    #[test]
    fn test_denorm_map_equatable_to_setlike() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let u = Type::unspec("u");
        let equatable = Type::var(&builder.add_type("Equatable"));
        let setlike = builder.add_type("SetLike");
        let map = builder.add_type("Map");

        builder.add_implies(
            Type::and_pair(
                &equatable,
                &Type::ground_kv(&map, [
                    ("key", Type::and_pair(&t, &equatable)),
                    ("val", u.clone()),
                ].into_iter()),
            ),
            Type::ground_kv(&setlike, [("item", t.clone())].into_iter()),
        ).unwrap();

        let str_ = Type::var(&builder.add_type("Str"));
        let bool_ = Type::var(&builder.add_type("Bool"));
        let _ = builder.build();

        let input = Type::and_pair(
            &equatable,
            &Type::ground_kv(&map, [
                ("key", Type::and_pair(&str_, &equatable)),
                ("val", bool_),
            ].into_iter()),
        );

        let expected = Type::ground_kv(&setlike, [("item", str_.clone())].into_iter());
        let denormed = input.denorm().unwrap();

        println!("{}", denormed);
        assert!(denormed.matches(&expected).unwrap());
        assert!(denormed.matches(&input).unwrap());
    }

    #[test]
    fn test_denorm_buffered_stream_conversion() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let stream = Type::var(&builder.add_type("Stream"));
        let buffered_stream = builder.add_type("BufferedStream");
        let buffer = builder.add_type("Buffer");

        builder.add_implies(
            Type::and_pair(
                &stream,
                &Type::ground_kv(&buffer, [("source", Type::and_pair(&t, &stream))].into_iter()),
            ),
            Type::ground_kv(&buffered_stream, [("base", t.clone())].into_iter()),
        ).unwrap();

        let socket = Type::var(&builder.add_type("Socket"));
        let _ = builder.build();

        let input = Type::and_pair(
            &stream,
            &Type::ground_kv(&buffer, [("source", Type::and_pair(&socket, &stream))].into_iter()),
        );

        let expected = Type::ground_kv(&buffered_stream, [("base", socket.clone())].into_iter());
        let denormed = input.denorm().unwrap();

        println!("{}", denormed);
        assert!(denormed.matches(&expected).unwrap());
        assert!(denormed.matches(&input).unwrap());
    }

    #[test]
    fn test_denorm_chained_implications() {
        let mut builder = DialectBuilder::new("test");

        let a = Type::var(&builder.add_type("A"));
        let b = Type::var(&builder.add_type("B"));
        let c = Type::var(&builder.add_type("C"));

        builder.add_implies(a.clone(), b.clone()).unwrap();
        builder.add_implies(b.clone(), c.clone()).unwrap();

        let _ = builder.build();

        let denormed = a.denorm().unwrap();

        assert!(denormed.matches(&b).unwrap());
        assert!(denormed.matches(&c).unwrap());
        assert!(denormed.matches(&a).unwrap());
    }

    #[test]
    fn test_denorm_ground_with_wildcard_implies_trait() {
        let mut builder = DialectBuilder::new("test");

        let marker = Type::var(&builder.add_type("Marker"));
        let wrapper = builder.add_type("Wrapper");
        let u64 = Type::var(&builder.add_type("U64"));

        // Wrapper{inner: ?} implies Marker
        builder.add_implies(
            Type::ground_kv(&wrapper, [("inner", Type::any())].into_iter()),
            marker.clone(),
        ).unwrap();

        let _ = builder.build();

        let wrapped_u64 = Type::ground_kv(&wrapper, [("inner", u64.clone())].into_iter());
        let denormed = wrapped_u64.denorm().unwrap();

        assert!(denormed.matches(&marker).unwrap());
        assert!(denormed.matches(&wrapped_u64).unwrap());
        assert!(!u64.matches(&marker).unwrap()); // not a match unless wrapped
    }

    #[test]
    fn test_denorm_irrelevant_implication() {
        let mut builder = DialectBuilder::new("test");

        let unrelated = Type::var(&builder.add_type("Unrelated"));
        let target = Type::var(&builder.add_type("Target"));
        let dummy = Type::var(&builder.add_type("Dummy"));

        // Dummy implies Unrelated
        builder.add_implies(dummy.clone(), unrelated.clone()).unwrap();

        let _ = builder.build();

        let denormed = target.denorm().unwrap();

        assert!(!denormed.matches(&unrelated).unwrap());
        assert!(denormed.matches(&target).unwrap());
    }

    #[test]
    fn test_denorm_identity_preserved() {
        let mut builder = DialectBuilder::new("test");

        let a = Type::var(&builder.add_type("A"));

        let _ = builder.build();

        let denormed = a.denorm().unwrap();

        assert!(denormed.matches(&a).unwrap());
    }

    #[test]
    fn test_denorm_multiple_matching_rules() {
        let mut builder = DialectBuilder::new("test");

        let base = Type::var(&builder.add_type("Base"));
        let trait1 = Type::var(&builder.add_type("Trait1"));
        let trait2 = Type::var(&builder.add_type("Trait2"));

        builder.add_implies(base.clone(), trait1.clone()).unwrap();
        builder.add_implies(base.clone(), trait2.clone()).unwrap();

        let _ = builder.build();

        let denormed = base.denorm().unwrap();

        assert!(denormed.matches(&trait1).unwrap());
        assert!(denormed.matches(&trait2).unwrap());
        assert!(denormed.matches(&base).unwrap());
    }

    #[test]
    fn test_denorm_recursive_generic_chain() {
        let mut builder = DialectBuilder::new("test");

        let box_ = builder.add_type("Box");
        let printable = Type::var(&builder.add_type("Printable"));

        // Box{val: t} implies Printable if t is Printable
        builder.add_implies(
            Type::ground_kv(&box_, [("val", printable.clone())].into_iter()),
            printable.clone(),
        ).unwrap();

        let u64 = Type::var(&builder.add_type("U64"));
        builder.add_implies(u64.clone(), printable.clone()).unwrap();

        let _ = builder.build();

        let boxed_u64 = Type::ground_kv(&box_, [("val", u64.clone())].into_iter());

        let denormed = boxed_u64.denorm().unwrap();
        assert!(denormed.matches(&printable).unwrap());
    }

    #[test]
    fn test_denorm_multiple_ground_fields() {
        let mut builder = DialectBuilder::new("test");

        let k = Type::unspec("k");
        let v = Type::unspec("v");
        let map = builder.add_type("Map");
        let iterable = builder.add_type("Iterable");

        // Map{key: k, val: v} implies Iterable{elt: v}
        builder.add_implies(
            Type::ground_kv(&map, [("key", k.clone()), ("val", v.clone())].into_iter()),
            Type::ground_kv(&iterable, [("elt", v.clone())].into_iter()),
        ).unwrap();

        let str_ = Type::var(&builder.add_type("Str"));
        let num = Type::var(&builder.add_type("Num"));

        let _ = builder.build();

        let map_type = Type::ground_kv(&map, [("key", str_.clone()), ("val", num.clone())].into_iter());
        let expected_iter = Type::ground_kv(&iterable, [("elt", num.clone())].into_iter());

        let denormed = map_type.denorm().unwrap();
        assert!(denormed.matches(&expected_iter).unwrap());
    }

    #[test]
    fn test_denorm_redundant_implication() {
        let mut builder = DialectBuilder::new("test");

        let foo = Type::var(&builder.add_type("Foo"));
        let bar = Type::var(&builder.add_type("Bar"));

        builder.add_implies(foo.clone(), bar.clone()).unwrap();
        builder.add_implies(foo.clone(), bar.clone()).unwrap(); // duplicated rule

        let _ = builder.build();

        let denormed = foo.denorm().unwrap();
        assert!(denormed.matches(&bar).unwrap());
    }

    #[test]
    fn test_denorm_multiple_unspec_nested() {
        let mut builder = DialectBuilder::new("test");

        let k = Type::unspec("k");
        let v = Type::unspec("v");
        let list = builder.add_type("List");
        let dict = builder.add_type("Dict");
        let flattens_to = builder.add_type("FlatList");

        // Dict{key: k, val: List{elt: v}} implies FlatList{elt: v}
        builder.add_implies(
            Type::ground_kv(&dict, [("key", k.clone()), ("val", Type::ground_kv(&list, [("elt", v.clone())].into_iter()))].into_iter()),
            Type::ground_kv(&flattens_to, [("elt", v.clone())].into_iter()),
        ).unwrap();

        let str_ = Type::var(&builder.add_type("Str"));
        let num = Type::var(&builder.add_type("Num"));

        let _ = builder.build();

        let nested = Type::ground_kv(&dict, [
            ("key", str_.clone()),
            ("val", Type::ground_kv(&list, [("elt", num.clone())].into_iter()))
        ].into_iter());

        let expected = Type::ground_kv(&flattens_to, [("elt", num.clone())].into_iter());

        let denormed = nested.denorm().unwrap();
        assert!(denormed.matches(&expected).unwrap());
    }

    #[test]
    fn test_denorm_unspec_invalid() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let vec = builder.add_type("Vec");
        let iter = builder.add_type("Iterable");

        // Vec{elt: t} implies Iterable{elt: t}
        builder.add_implies(
            Type::ground_kv(&vec, [("elt", t.clone())].into_iter()),
            Type::ground_kv(&iter, [("elt", t.clone())].into_iter()),
        ).unwrap();

        let _ = builder.build();

        let abstract_vec = Type::ground_kv(&vec, [("elt", t.clone())].into_iter());

        let _ = abstract_vec.denorm().unwrap_err();
    }

    #[test]
    fn test_denorm_nested_box_box_to_flattened() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let box_ = builder.add_type("Box");
        let flattened = Type::var(&builder.add_type("Flattened"));

        // Box{val: Box{val: t}} implies Flattened
        builder.add_implies(
            Type::ground_kv(&box_, [(
                "val",
                Type::ground_kv(&box_, [("val", t.clone())].into_iter())
            )].into_iter()),
            flattened.clone(),
        ).unwrap();

        let u64 = Type::var(&builder.add_type("U64"));

        let _ = builder.build();

        let nested_box = Type::ground_kv(&box_, [(
            "val",
            Type::ground_kv(&box_, [("val", u64.clone())].into_iter())
        )].into_iter());

        let denormed = nested_box.denorm().unwrap();

        assert!(denormed.matches(&flattened).unwrap());
        assert!(denormed.matches(&nested_box).unwrap());
    }

    #[test]
    fn test_denorm_deeply_nested_composite_to_trait() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let option = builder.add_type("Option");
        let result = builder.add_type("Result");
        let clone = Type::var(&builder.add_type("Clone"));

        // Option{val: Result{ok: t, err: t}} implies Clone
        builder.add_implies(
            Type::ground_kv(&option, [(
                "val",
                Type::ground_kv(&result, [
                    ("ok", t.clone()),
                    ("err", t.clone())
                ].into_iter())
            )].into_iter()),
            clone.clone(),
        ).unwrap();

        let str_ = Type::var(&builder.add_type("Str"));

        let _ = builder.build();

        let nested = Type::ground_kv(&option, [(
            "val",
            Type::ground_kv(&result, [
                ("ok", str_.clone()),
                ("err", str_.clone())
            ].into_iter())
        )].into_iter());

        let denormed = nested.denorm().unwrap();

        assert!(denormed.matches(&clone).unwrap());
        assert!(denormed.matches(&nested).unwrap());
        assert!(!str_.matches(&clone).unwrap());
    }

    #[test]
    fn test_denorm_recursive_wrapper() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let wrapper = builder.add_type("Wrapper");
        let marker = Type::var(&builder.add_type("Marked"));

        // Wrapper{inner: Wrapper{inner: t}} implies Marked
        builder.add_implies(
            Type::ground_kv(&wrapper, [(
                "inner",
                Type::ground_kv(&wrapper, [("inner", t.clone())].into_iter())
            )].into_iter()),
            marker.clone(),
        ).unwrap();

        let bool_ = Type::var(&builder.add_type("Bool"));

        let _ = builder.build();

        let nested = Type::ground_kv(&wrapper, [(
            "inner",
            Type::ground_kv(&wrapper, [("inner", bool_.clone())].into_iter())
        )].into_iter());

        let denormed = nested.denorm().unwrap();

        assert!(denormed.matches(&marker).unwrap());
        assert!(denormed.matches(&nested).unwrap());
    }

    #[test]
    fn test_denorm_nested_generic_with_propagation() {
        let mut builder = DialectBuilder::new("test");

        let t = Type::unspec("t");
        let outer = builder.add_type("Outer");
        let inner = builder.add_type("Inner");
        let access = builder.add_type("Access");

        // Inner{val: t} implies Access{target: t}
        builder.add_implies(
            Type::ground_kv(&inner, [("val", t.clone())].into_iter()),
            Type::ground_kv(&access, [("target", t.clone())].into_iter()),
        ).unwrap();

        // Outer{inner: Inner{val: t}} implies Access{target: t}
        builder.add_implies(
            Type::ground_kv(&outer, [(
                "inner",
                Type::ground_kv(&inner, [("val", t.clone())].into_iter())
            )].into_iter()),
            Type::ground_kv(&access, [("target", t.clone())].into_iter()),
        ).unwrap();

        let i8 = Type::var(&builder.add_type("I8"));

        let _ = builder.build();

        let composed = Type::ground_kv(&outer, [(
            "inner",
            Type::ground_kv(&inner, [("val", i8.clone())].into_iter())
        )].into_iter());

        let expected = Type::ground_kv(&access, [("target", i8.clone())].into_iter());

        let denormed = composed.denorm().unwrap();

        assert!(denormed.matches(&expected).unwrap());
        assert!(denormed.matches(&composed).unwrap());
    }
}
