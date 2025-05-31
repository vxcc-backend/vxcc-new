use crate::{*, types::*};

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

#[test]
fn test_denorm_self_referential_rule() {
    let mut builder = DialectBuilder::new("test");

    let a = Type::var(&builder.add_type("A"));

    // A implies A (bad rule, should not recurse infinitely)
    builder.add_implies(a.clone(), a.clone()).unwrap();

    let _ = builder.build();

    let denormed = a.denorm().unwrap();
    assert!(denormed.matches(&a).unwrap());
}

#[test]
fn test_denorm_mutual_recursion() {
    let mut builder = DialectBuilder::new("test");

    let a = Type::var(&builder.add_type("A"));
    let b = Type::var(&builder.add_type("B"));

    builder.add_implies(a.clone(), b.clone()).unwrap();
    builder.add_implies(b.clone(), a.clone()).unwrap();

    let _ = builder.build();

    let denorm_a = a.denorm().unwrap();
    let denorm_b = b.denorm().unwrap();

    assert!(denorm_a.matches(&b).unwrap());
    assert!(denorm_b.matches(&a).unwrap());
}

#[test]
fn test_denorm_nested_wildcard_explosion() {
    let mut builder = DialectBuilder::new("test");

    let wrapper = builder.add_type("Wrapper");
    let anything = Type::var(&builder.add_type("Anything"));

    // Wrapper{val: ?} implies Anything
    builder.add_implies(
        Type::ground_kv(&wrapper, [("val", Type::any())].into_iter()),
        anything.clone(),
    ).unwrap();
    let u64 = Type::var(&builder.add_type("U64"));
    let _ = builder.build();

    let wrapped = Type::ground_kv(&wrapper, [("val", u64.clone())].into_iter());

    let denormed = wrapped.denorm().unwrap();

    assert!(denormed.matches(&anything).unwrap());
    assert!(denormed.matches(&wrapped).unwrap());
}

#[test]
fn test_denorm_recursive_structure_vec_to_vecvec() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");
    let vec = builder.add_type("Vec");

    // Vec{elt: t} ⇒ Vec{elt: Vec{elt: t}} (pathological!)
    builder.add_implies(
        Type::ground_kv(&vec, [("elt", t.clone())].into_iter()),
        Type::ground_kv(&vec, [(
            "elt",
            Type::ground_kv(&vec, [("elt", t.clone())].into_iter())
        )].into_iter()),
    ).unwrap();

    let u8 = Type::var(&builder.add_type("U8"));
    let input = Type::ground_kv(&vec, [("elt", u8.clone())].into_iter());

    let _ = builder.build();

    let denormed = input.denorm().unwrap();

    // Should not recurse infinitely, but only one expansion step
    let expected = Type::ground_kv(&vec, [(
        "elt",
        Type::ground_kv(&vec, [("elt", u8.clone())].into_iter())
    )].into_iter());

    assert!(denormed.matches(&expected).unwrap());
}

#[test]
fn test_denorm_unspec_combination_explosion() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");
    let container = builder.add_type("Container");
    let has_value = Type::var(&builder.add_type("HasValue"));
    let is_valid = Type::var(&builder.add_type("IsValid"));

    // Container{item: t} ⇒ HasValue
    builder.add_implies(
        Type::ground_kv(&container, [("item", t.clone())].into_iter()),
        has_value.clone(),
    ).unwrap();

    // Container{item: t} ⇒ IsValid
    builder.add_implies(
        Type::ground_kv(&container, [("item", t.clone())].into_iter()),
        is_valid.clone(),
    ).unwrap();

    let bool_ = Type::var(&builder.add_type("Bool"));
    let input = Type::ground_kv(&container, [("item", bool_.clone())].into_iter());

    let _ = builder.build();

    let denormed = input.denorm().unwrap();
    assert!(denormed.matches(&has_value).unwrap());
    assert!(denormed.matches(&is_valid).unwrap());
}

#[test]
fn test_denorm_intersection_recursive_loop() {
    let mut builder = DialectBuilder::new("test");

    let a = Type::var(&builder.add_type("A"));
    let b = Type::var(&builder.add_type("B"));
    let c = Type::var(&builder.add_type("C"));

    // A + B ⇒ C
    builder.add_implies(
        Type::and_pair(&a, &b),
        c.clone(),
    ).unwrap();

    // C ⇒ A
    builder.add_implies(c.clone(), a.clone()).unwrap();

    let input = Type::and_pair(&a, &b);

    let _ = builder.build();

    let denormed = input.denorm().unwrap();

    // Should still terminate and include C and A again
    assert!(denormed.matches(&c).unwrap());
    assert!(denormed.matches(&a).unwrap());
}
