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

pub type TypeVar = Arc<TypeVarImpl>;

#[derive(Eq, PartialEq, Hash)]
pub struct TypeAnd {
    conjugate: Vec<Type>
}

impl std::fmt::Display for TypeAnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, x) in self.get_all().enumerate() {
            if idx > 0 {
                write!(f, " + ")?;
            }

            // need parenthesis arround inner TAnd (which shouldn't even exist!)
            match *x.0 {
                TypeImpl::Any        |
                TypeImpl::Var(_)     |
                TypeImpl::NumList(_) |
                TypeImpl::Ground(_) => write!(f, "{}", x)?,

                TypeImpl::And(_) => write!(f, "({})", x)?,
            }
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
                TypeImpl::Any => {},

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
                        .filter_map(|x| match &*x.0 {
                            TypeImpl::Ground(g) => Some(g),
                            _ => None,
                        })
                        .find_position(|xg| xg.tag == g.tag);

                    if let Some((idx, xg)) = ptr {
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
        write!(f, "{}{{", self.tag)?;
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
}

impl std::fmt::Display for TypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeImpl::Any => write!(f, "?"),
            TypeImpl::Var(inner) => write!(f, "{}", inner),
            TypeImpl::And(inner) => write!(f, "{}", inner),
            TypeImpl::Ground(inner) => write!(f, "{}", inner),
            TypeImpl::NumList(inner) => write!(f, "{}", inner),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash)]
pub struct Type(Arc<TypeImpl>);

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

impl Type {
    pub fn any() -> Self {
        Self::from(TypeImpl::Any)
    }

    pub fn var(name: &TypeVar) -> Self {
        Self::from(TypeImpl::Var(name.clone()))
    }

    pub fn and<I: Iterator<Item = Type>>(from: I) -> Self {
        Self::from(TypeImpl::And(TypeAnd::from(from)))
    }

    pub fn and_pair(&self, b: &Self) -> Self {
        Self::and([self.clone(), b.clone()].into_iter())
    }

    /// can be used for named type arguments, and structs
    ///
    /// if there are no arguments, this simplifies to [Self::var]
    pub fn ground_kv<'a, I: Iterator<Item = (&'a str, Type)>>(tag: &TypeVar, from: I) -> Self {
        let inner: HashMap<_,_> = from
            .map(|(k,v)| (String::from(k), v))
            .collect();

        if inner.is_empty() {
            Self::var(tag)
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

    /// applies all "implies" statements from all dialects to all inner types.
    pub fn denorm(&self) -> Self {
        // TODO
        // dialects that own eithet the left side or the right side type of a implies, can create implies
        self.clone()
    }

    /// does this type have all types from the other type?
    ///
    /// this calls [Self::denorm] first
    pub fn matches(&self, other: &Self) -> bool {
        self.denorm().fast_matches(other)
    }

    fn fast_matches(&self, other: &Self) -> bool {
        fn match_ground(this: &TypeGround, other: &TypeGround) -> bool {
            if this.tag != other.tag {
                return false;
            }

            for (k,v) in other.get_params().into_iter() {
                let tv = this.get_param(k.as_str());
                if let Some(tv) = tv {
                    if !tv.fast_matches(&v) {
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

            TypeImpl::Var(var) => match &*self.0 {
                TypeImpl::Var(x) => *var == *x,
                TypeImpl::And(x) => x.get_all().any(|x| x.fast_matches(other)),
                _ => false
            }

            TypeImpl::And(and) => {
                and.get_all().all(|x| self.fast_matches(&x))
            }

            TypeImpl::Ground(ground) => {
                match &*self.0 {
                    TypeImpl::Ground(x) => {
                        match_ground(x, ground)
                    }

                    TypeImpl::And(and) => {
                        and.get_all()
                            .any(|item| match &*item.0 {
                                TypeImpl::Ground(x) => {
                                    match_ground(x, ground)
                                }

                                _ => false,
                            })
                    }

                    _ => false
                }
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
        let test_dialect = DialectBuilder::new("test");
        let ty_i8 = Type::var(&test_dialect.add_type("I8"));
        let ty_num = Type::var(&test_dialect.add_type("Number"));
        let ty_uint = Type::var(&test_dialect.add_type("UInt"));
        let _test_dialect = test_dialect.build().get_dialect();

        let uni = Type::and_pair(&ty_i8, &ty_num).unify(&ty_uint);
        assert!(uni.matches(&Type::and_pair(&Type::and_pair(&ty_i8, &ty_uint), &ty_num)));
        assert!(uni.matches(&ty_i8));
        assert!(uni.matches(&ty_num));
        assert!(uni.matches(&ty_uint));
        assert!(uni.matches(&Type::and_pair(&ty_uint, &ty_num)));
        assert!(!uni.matches(&Type::var(&core_dialect::DIALECT.clone)));
        assert!(!ty_i8.matches(&ty_num));
    }
}
