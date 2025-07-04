use crate::{types::*, *};

#[test]
fn test_simple_unify() {
    let mut test_dialect = DialectBuilder::new("test");
    let ty_i8 = Type::var(&test_dialect.add_type("I8"));
    let ty_num = Type::var(&test_dialect.add_type("Number"));
    let ty_uint = Type::var(&test_dialect.add_type("UInt"));
    let _test_dialect = test_dialect.build().get_dialect();

    let uni = Type::and_pair(&ty_i8, &ty_num)
        .unwrap()
        .unify(&ty_uint)
        .unwrap();
    assert!(
        uni.matches(&Type::and_pair(&Type::and_pair(&ty_i8, &ty_uint).unwrap(), &ty_num).unwrap())
            .unwrap()
    );
    assert!(uni.matches(&ty_i8).unwrap());
    assert!(uni.matches(&ty_num).unwrap());
    assert!(uni.matches(&ty_uint).unwrap());
    assert!(
        uni.matches(&Type::and_pair(&ty_uint, &ty_num).unwrap())
            .unwrap()
    );
    assert!(
        !uni.matches(&Type::var(&vxcc_core_dialect::DIALECT.types.Clone))
            .unwrap()
    );
    assert!(!ty_i8.matches(&ty_num).unwrap());
}

#[test]
fn test_denorm_and_implies_clone() {
    let mut builder = DialectBuilder::new("test");

    let clone = Type::var(&builder.add_type("Clone"));
    let num = Type::var(&builder.add_type("Num"));
    let u64 = Type::var(&builder.add_type("U64"));

    builder.add_implies(u64.clone(), clone.clone()).unwrap();

    let _ = builder.build();

    let orig = Type::and_pair(&num, &u64).unwrap();
    let denormed = orig.denorm().unwrap();

    assert!(denormed.matches(&clone).unwrap());
    assert!(denormed.matches(&orig).unwrap());
}

#[test]
fn test_denorm_vector_implies_clone() {
    let mut builder = DialectBuilder::new("test");

    let clone = Type::var(&builder.add_type("Clone"));
    let vector = builder.add_ground_type("Vector", ["elt"].into_iter());
    let vector_elt = vector.ground_arg("elt").unwrap();
    let u64 = Type::var(&builder.add_type("U64"));

    builder
        .add_implies(
            Type::ground_kv(&vector, [(vector_elt.clone(), Type::any())].into_iter()).unwrap(),
            clone.clone(),
        )
        .unwrap();

    let _ = builder.build();

    let vec_u64 = Type::ground_kv(&vector, [(vector_elt, u64.clone())].into_iter()).unwrap();
    let denormed = vec_u64.denorm().unwrap();

    assert!(denormed.matches(&clone).unwrap());
    assert!(denormed.matches(&vec_u64).unwrap());
    assert!(!clone.matches(&vec_u64).unwrap()); // Non-symmetric
}

#[test]
fn test_denorm_template_preserves_parameter() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");
    let vector = builder.add_ground_type("Vector", ["elt"].into_iter());
    let vector_elt = vector.ground_arg("elt").unwrap();
    let iterable = builder.add_ground_type("Iterable", ["elt"].into_iter());
    let iterable_elt = iterable.ground_arg("elt").unwrap();

    builder
        .add_implies(
            Type::ground_kv(&vector, [(vector_elt.clone(), t.clone())].into_iter()).unwrap(),
            Type::ground_kv(&iterable, [(iterable_elt.clone(), t.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    let u64 = Type::var(&builder.add_type("U64"));
    let _ = builder.build();

    let vec_u64 =
        Type::ground_kv(&vector, [(vector_elt.clone(), u64.clone())].into_iter()).unwrap();
    let iter_u64 =
        Type::ground_kv(&iterable, [(iterable_elt.clone(), u64.clone())].into_iter()).unwrap();

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
    let vector = builder.add_ground_type("Vector", ["elt"].into_iter());
    let vector_elt = vector.ground_arg("elt").unwrap();
    let iterable = builder.add_ground_type("Iterable", ["elt"].into_iter());
    let iterable_elt = iterable.ground_arg("elt").unwrap();

    builder
        .add_implies(
            Type::and_pair(
                &clone,
                &Type::ground_kv(
                    &vector,
                    [(vector_elt.clone(), Type::and_pair(&t, &clone).unwrap())].into_iter(),
                )
                .unwrap(),
            )
            .unwrap(),
            Type::ground_kv(&iterable, [(iterable_elt.clone(), t.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    builder
        .add_implies(
            Type::and_pair(
                &clone,
                &Type::ground_kv(
                    &vector,
                    [(vector_elt.clone(), Type::and_pair(&t, &clone).unwrap())].into_iter(),
                )
                .unwrap(),
            )
            .unwrap(),
            idfk.clone(),
        )
        .unwrap();

    let u64 = Type::var(&builder.add_type("U64"));
    let _ = builder.build();

    let input = Type::and_pair(
        &clone,
        &Type::ground_kv(
            &vector,
            [(vector_elt.clone(), Type::and_pair(&u64, &clone).unwrap())].into_iter(),
        )
        .unwrap(),
    )
    .unwrap();

    let iter_u64 =
        Type::ground_kv(&iterable, [(iterable_elt.clone(), u64.clone())].into_iter()).unwrap();
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
    let wrapper = builder.add_ground_type("Wrapper", ["inner"].into_iter());
    let wrapper_inner = wrapper.ground_arg("inner").unwrap();

    let lhs = Type::and_pair(
        &clone,
        &Type::ground_kv(
            &wrapper,
            [(wrapper_inner.clone(), Type::and_pair(&t, &clone).unwrap())].into_iter(),
        )
        .unwrap(),
    )
    .unwrap();

    builder
        .add_implies(lhs.clone(), serializable.clone())
        .unwrap();

    builder.add_implies(lhs, debuggable.clone()).unwrap();

    let str_ = Type::var(&builder.add_type("Str"));
    let _ = builder.build();

    let input = Type::and_pair(
        &clone,
        &Type::ground_kv(
            &wrapper,
            [(
                wrapper_inner.clone(),
                Type::and_pair(&str_, &clone).unwrap(),
            )]
            .into_iter(),
        )
        .unwrap(),
    )
    .unwrap();

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
    let box_ = builder.add_ground_type("Box", ["val"].into_iter());
    let box_val = box_.ground_arg("val").unwrap();

    builder
        .add_implies(
            Type::and_pair(
                &readable,
                &Type::ground_kv(
                    &box_,
                    [(box_val.clone(), Type::and_pair(&t, &readable).unwrap())].into_iter(),
                )
                .unwrap(),
            )
            .unwrap(),
            decodable.clone(),
        )
        .unwrap();

    let data = Type::var(&builder.add_type("Data"));
    let _ = builder.build();

    let input = Type::and_pair(
        &readable,
        &Type::ground_kv(
            &box_,
            [(box_val, Type::and_pair(&data, &readable).unwrap())].into_iter(),
        )
        .unwrap(),
    )
    .unwrap();

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
    let setlike = builder.add_ground_type("SetLike", ["item"].into_iter());
    let setlike_item = setlike.ground_arg("item").unwrap();
    let map = builder.add_ground_type("Map", ["key", "val"].into_iter());
    let map_key = map.ground_arg("key").unwrap();
    let map_val = map.ground_arg("val").unwrap();

    builder
        .add_implies(
            Type::and_pair(
                &equatable,
                &Type::ground_kv(
                    &map,
                    [
                        (map_key.clone(), Type::and_pair(&t, &equatable).unwrap()),
                        (map_val.clone(), u.clone()),
                    ]
                    .into_iter(),
                )
                .unwrap(),
            )
            .unwrap(),
            Type::ground_kv(&setlike, [(setlike_item.clone(), t.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    let str_ = Type::var(&builder.add_type("Str"));
    let bool_ = Type::var(&builder.add_type("Bool"));
    let _ = builder.build();

    let input = Type::and_pair(
        &equatable,
        &Type::ground_kv(
            &map,
            [
                (map_key.clone(), Type::and_pair(&str_, &equatable).unwrap()),
                (map_val.clone(), bool_),
            ]
            .into_iter(),
        )
        .unwrap(),
    )
    .unwrap();

    let expected =
        Type::ground_kv(&setlike, [(setlike_item.clone(), str_.clone())].into_iter()).unwrap();
    let denormed = input.denorm().unwrap();

    assert!(denormed.matches(&expected).unwrap());
    assert!(denormed.matches(&input).unwrap());
}

#[test]
fn test_denorm_buffered_stream_conversion() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");
    let stream = Type::var(&builder.add_type("Stream"));

    let buffered_stream = builder.add_ground_type("BufferedStream", ["base"].into_iter());
    let buffered_stream_base = buffered_stream.ground_arg("base").unwrap();

    let buffer = builder.add_ground_type("Buffer", ["source"].into_iter());
    let buffer_source = buffer.ground_arg("source").unwrap();

    builder
        .add_implies(
            Type::and_pair(
                &stream,
                &Type::ground_kv(
                    &buffer,
                    [(buffer_source.clone(), Type::and_pair(&t, &stream).unwrap())].into_iter(),
                )
                .unwrap(),
            )
            .unwrap(),
            Type::ground_kv(
                &buffered_stream,
                [(buffered_stream_base.clone(), t.clone())].into_iter(),
            )
            .unwrap(),
        )
        .unwrap();

    let socket = Type::var(&builder.add_type("Socket"));
    let _ = builder.build();

    let input = Type::and_pair(
        &stream,
        &Type::ground_kv(
            &buffer,
            [(
                buffer_source.clone(),
                Type::and_pair(&socket, &stream).unwrap(),
            )]
            .into_iter(),
        )
        .unwrap(),
    )
    .unwrap();

    let expected = Type::ground_kv(
        &buffered_stream,
        [(buffered_stream_base.clone(), socket.clone())].into_iter(),
    )
    .unwrap();
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
    let wrapper = builder.add_ground_type("Wrapper", ["inner"].into_iter());
    let wrapper_inner = wrapper.ground_arg("inner").unwrap();
    let u64 = Type::var(&builder.add_type("U64"));

    // Wrapper{inner: ?} implies Marker
    builder
        .add_implies(
            Type::ground_kv(&wrapper, [(wrapper_inner.clone(), Type::any())].into_iter()).unwrap(),
            marker.clone(),
        )
        .unwrap();

    let _ = builder.build();

    let wrapped_u64 =
        Type::ground_kv(&wrapper, [(wrapper_inner.clone(), u64.clone())].into_iter()).unwrap();
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
    builder
        .add_implies(dummy.clone(), unrelated.clone())
        .unwrap();

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

    let box_ = builder.add_ground_type("Box", ["val"].into_iter());
    let box_val = box_.ground_arg("val").unwrap();

    let printable = Type::var(&builder.add_type("Printable"));

    // Box{val: t} implies Printable if t is Printable
    builder
        .add_implies(
            Type::ground_kv(&box_, [(box_val.clone(), printable.clone())].into_iter()).unwrap(),
            printable.clone(),
        )
        .unwrap();

    let u64 = Type::var(&builder.add_type("U64"));
    builder.add_implies(u64.clone(), printable.clone()).unwrap();

    let _ = builder.build();

    let boxed_u64 = Type::ground_kv(&box_, [(box_val.clone(), u64.clone())].into_iter()).unwrap();

    let denormed = boxed_u64.denorm().unwrap();
    assert!(denormed.matches(&printable).unwrap());
}

#[test]
fn test_denorm_multiple_ground_fields() {
    let mut builder = DialectBuilder::new("test");

    let k = Type::unspec("k");
    let v = Type::unspec("v");

    let map = builder.add_ground_type("Map", ["key", "val"].into_iter());
    let map_key = map.ground_arg("key").unwrap();
    let map_val = map.ground_arg("val").unwrap();

    let iterable = builder.add_ground_type("Iterable", ["elt"].into_iter());
    let iterable_elt = iterable.ground_arg("elt").unwrap();

    // Map{key: k, val: v} implies Iterable{elt: v}
    builder
        .add_implies(
            Type::ground_kv(
                &map,
                [(map_key.clone(), k.clone()), (map_val.clone(), v.clone())].into_iter(),
            )
            .unwrap(),
            Type::ground_kv(&iterable, [(iterable_elt.clone(), v.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    let str_ = Type::var(&builder.add_type("Str"));
    let num = Type::var(&builder.add_type("Num"));

    let _ = builder.build();

    let map_type = Type::ground_kv(
        &map,
        [
            (map_key.clone(), str_.clone()),
            (map_val.clone(), num.clone()),
        ]
        .into_iter(),
    )
    .unwrap();
    let expected_iter =
        Type::ground_kv(&iterable, [(iterable_elt.clone(), num.clone())].into_iter()).unwrap();

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

    let list = builder.add_ground_type("List", ["elt"].into_iter());
    let list_elt = list.ground_arg("elt").unwrap();

    let dict = builder.add_ground_type("Dict", ["key", "val"].into_iter());
    let dict_key = dict.ground_arg("key").unwrap();
    let dict_val = dict.ground_arg("val").unwrap();

    let flattens_to = builder.add_ground_type("FlatList", ["elt"].into_iter());
    let flattens_to_elt = flattens_to.ground_arg("elt").unwrap();

    // Dict{key: k, val: List{elt: v}} implies FlatList{elt: v}
    builder
        .add_implies(
            Type::ground_kv(
                &dict,
                [
                    (dict_key.clone(), k.clone()),
                    (
                        dict_val.clone(),
                        Type::ground_kv(&list, [(list_elt.clone(), v.clone())].into_iter())
                            .unwrap(),
                    ),
                ]
                .into_iter(),
            )
            .unwrap(),
            Type::ground_kv(
                &flattens_to,
                [(flattens_to_elt.clone(), v.clone())].into_iter(),
            )
            .unwrap(),
        )
        .unwrap();

    let str_ = Type::var(&builder.add_type("Str"));
    let num = Type::var(&builder.add_type("Num"));

    let _ = builder.build();

    let nested = Type::ground_kv(
        &dict,
        [
            (dict_key.clone(), str_.clone()),
            (
                dict_val.clone(),
                Type::ground_kv(&list, [(list_elt.clone(), num.clone())].into_iter()).unwrap(),
            ),
        ]
        .into_iter(),
    )
    .unwrap();

    let expected = Type::ground_kv(
        &flattens_to,
        [(flattens_to_elt.clone(), num.clone())].into_iter(),
    )
    .unwrap();

    let denormed = nested.denorm().unwrap();
    assert!(denormed.matches(&expected).unwrap());
}

#[test]
fn test_denorm_unspec_invalid() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");

    let vec = builder.add_ground_type("Vec", ["elt"].into_iter());
    let vec_elt = vec.ground_arg("elt").unwrap();

    let iter = builder.add_ground_type("Iterable", ["elt"].into_iter());
    let iter_elt = iter.ground_arg("elt").unwrap();

    // Vec{elt: t} implies Iterable{elt: t}
    builder
        .add_implies(
            Type::ground_kv(&vec, [(vec_elt.clone(), t.clone())].into_iter()).unwrap(),
            Type::ground_kv(&iter, [(iter_elt.clone(), t.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    let _ = builder.build();

    let abstract_vec = Type::ground_kv(&vec, [(vec_elt.clone(), t.clone())].into_iter()).unwrap();

    let _ = abstract_vec.denorm().unwrap_err();
}

#[test]
fn test_denorm_nested_box_box_to_flattened() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");

    let box_ = builder.add_ground_type("Box", ["val"].into_iter());
    let box_val = box_.ground_arg("val").unwrap();

    let flattened = Type::var(&builder.add_type("Flattened"));

    // Box{val: Box{val: t}} implies Flattened
    builder
        .add_implies(
            Type::ground_kv(
                &box_,
                [(
                    box_val.clone(),
                    Type::ground_kv(&box_, [(box_val.clone(), t.clone())].into_iter()).unwrap(),
                )]
                .into_iter(),
            )
            .unwrap(),
            flattened.clone(),
        )
        .unwrap();

    let u64 = Type::var(&builder.add_type("U64"));

    let _ = builder.build();

    let nested_box = Type::ground_kv(
        &box_,
        [(
            box_val.clone(),
            Type::ground_kv(&box_, [(box_val.clone(), u64.clone())].into_iter()).unwrap(),
        )]
        .into_iter(),
    )
    .unwrap();

    let denormed = nested_box.denorm().unwrap();

    assert!(denormed.matches(&flattened).unwrap());
    assert!(denormed.matches(&nested_box).unwrap());
}

#[test]
fn test_denorm_deeply_nested_composite_to_trait() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");

    let option = builder.add_ground_type("Option", ["val"].into_iter());
    let option_val = option.ground_arg("val").unwrap();

    let result = builder.add_ground_type("Result", ["ok", "err"].into_iter());
    let result_ok = result.ground_arg("ok").unwrap();
    let result_err = result.ground_arg("err").unwrap();

    let clone = Type::var(&builder.add_type("Clone"));

    // Option{val: Result{ok: t, err: t}} implies Clone
    builder
        .add_implies(
            Type::ground_kv(
                &option,
                [(
                    option_val.clone(),
                    Type::ground_kv(
                        &result,
                        [
                            (result_ok.clone(), t.clone()),
                            (result_err.clone(), t.clone()),
                        ]
                        .into_iter(),
                    )
                    .unwrap(),
                )]
                .into_iter(),
            )
            .unwrap(),
            clone.clone(),
        )
        .unwrap();

    let str_ = Type::var(&builder.add_type("Str"));

    let _ = builder.build();

    let nested = Type::ground_kv(
        &option,
        [(
            option_val.clone(),
            Type::ground_kv(
                &result,
                [
                    (result_ok.clone(), str_.clone()),
                    (result_err.clone(), str_.clone()),
                ]
                .into_iter(),
            )
            .unwrap(),
        )]
        .into_iter(),
    )
    .unwrap();

    let denormed = nested.denorm().unwrap();

    assert!(denormed.matches(&clone).unwrap());
    assert!(denormed.matches(&nested).unwrap());
    assert!(!str_.matches(&clone).unwrap());
}

#[test]
fn test_denorm_recursive_wrapper() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");
    let wrapper = builder.add_ground_type("Wrapper", ["inner"].into_iter());
    let wrapper_inner = wrapper.ground_arg("inner").unwrap();
    let marker = Type::var(&builder.add_type("Marked"));

    // Wrapper{inner: Wrapper{inner: t}} implies Marked
    builder
        .add_implies(
            Type::ground_kv(
                &wrapper,
                [(
                    wrapper_inner.clone(),
                    Type::ground_kv(&wrapper, [(wrapper_inner.clone(), t.clone())].into_iter())
                        .unwrap(),
                )]
                .into_iter(),
            )
            .unwrap(),
            marker.clone(),
        )
        .unwrap();

    let bool_ = Type::var(&builder.add_type("Bool"));

    let _ = builder.build();

    let nested = Type::ground_kv(
        &wrapper,
        [(
            wrapper_inner.clone(),
            Type::ground_kv(
                &wrapper,
                [(wrapper_inner.clone(), bool_.clone())].into_iter(),
            )
            .unwrap(),
        )]
        .into_iter(),
    )
    .unwrap();

    let denormed = nested.denorm().unwrap();

    assert!(denormed.matches(&marker).unwrap());
    assert!(denormed.matches(&nested).unwrap());
}

#[test]
fn test_denorm_nested_generic_with_propagation() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");

    let outer = builder.add_ground_type("Outer", ["inner"].into_iter());
    let outer_inner = outer.ground_arg("inner").unwrap();

    let inner = builder.add_ground_type("Inner", ["val"].into_iter());
    let inner_val = inner.ground_arg("val").unwrap();

    let access = builder.add_ground_type("Access", ["target"].into_iter());
    let access_target = access.ground_arg("target").unwrap();

    // Inner{val: t} implies Access{target: t}
    builder
        .add_implies(
            Type::ground_kv(&inner, [(inner_val.clone(), t.clone())].into_iter()).unwrap(),
            Type::ground_kv(&access, [(access_target.clone(), t.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    // Outer{inner: Inner{val: t}} implies Access{target: t}
    builder
        .add_implies(
            Type::ground_kv(
                &outer,
                [(
                    outer_inner.clone(),
                    Type::ground_kv(&inner, [(inner_val.clone(), t.clone())].into_iter()).unwrap(),
                )]
                .into_iter(),
            )
            .unwrap(),
            Type::ground_kv(&access, [(access_target.clone(), t.clone())].into_iter()).unwrap(),
        )
        .unwrap();

    let i8 = Type::var(&builder.add_type("I8"));

    let _ = builder.build();

    let composed = Type::ground_kv(
        &outer,
        [(
            outer_inner.clone(),
            Type::ground_kv(&inner, [(inner_val.clone(), i8.clone())].into_iter()).unwrap(),
        )]
        .into_iter(),
    )
    .unwrap();

    let expected =
        Type::ground_kv(&access, [(access_target.clone(), i8.clone())].into_iter()).unwrap();

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

    let wrapper = builder.add_ground_type("Wrapper", ["val"].into_iter());
    let wrapper_val = wrapper.ground_arg("val").unwrap();

    let anything = Type::var(&builder.add_type("Anything"));

    // Wrapper{val: ?} implies Anything
    builder
        .add_implies(
            Type::ground_kv(&wrapper, [(wrapper_val.clone(), Type::any())].into_iter()).unwrap(),
            anything.clone(),
        )
        .unwrap();
    let u64 = Type::var(&builder.add_type("U64"));
    let _ = builder.build();

    let wrapped =
        Type::ground_kv(&wrapper, [(wrapper_val.clone(), u64.clone())].into_iter()).unwrap();

    let denormed = wrapped.denorm().unwrap();

    assert!(denormed.matches(&anything).unwrap());
    assert!(denormed.matches(&wrapped).unwrap());
}

#[test]
fn test_denorm_unspec_combination_explosion() {
    let mut builder = DialectBuilder::new("test");

    let t = Type::unspec("t");
    let container = builder.add_ground_type("Container", ["item"].into_iter());
    let container_item = container.ground_arg("item").unwrap();
    let has_value = Type::var(&builder.add_type("HasValue"));
    let is_valid = Type::var(&builder.add_type("IsValid"));

    // Container{item: t} ⇒ HasValue
    builder
        .add_implies(
            Type::ground_kv(
                &container,
                [(container_item.clone(), t.clone())].into_iter(),
            )
            .unwrap(),
            has_value.clone(),
        )
        .unwrap();

    // Container{item: t} ⇒ IsValid
    builder
        .add_implies(
            Type::ground_kv(
                &container,
                [(container_item.clone(), t.clone())].into_iter(),
            )
            .unwrap(),
            is_valid.clone(),
        )
        .unwrap();

    let bool_ = Type::var(&builder.add_type("Bool"));
    let input = Type::ground_kv(
        &container,
        [(container_item.clone(), bool_.clone())].into_iter(),
    )
    .unwrap();

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
    builder
        .add_implies(Type::and_pair(&a, &b).unwrap(), c.clone())
        .unwrap();

    // C ⇒ A
    builder.add_implies(c.clone(), a.clone()).unwrap();

    let input = Type::and_pair(&a, &b).unwrap();

    let _ = builder.build();

    let denormed = input.denorm().unwrap();

    // Should still terminate and include C and A again
    assert!(denormed.matches(&c).unwrap());
    assert!(denormed.matches(&a).unwrap());
}
