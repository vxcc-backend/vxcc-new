use vxcc_dialect_parser::*;
use vxcc_ir::{Value, vxcc_core_dialect};
use vxcc_type_parser_proc::ty;

dialect! {
    dialect arith:
        core
        ;

    type U8;
    type U8 => core.Clone;

    type Vec { elt };

    type Iter { elt };

    type Vec { elt: ?e } => Iter { elt: ?elt };

    type Op;

    node add
        type Op
        ins
            a: U8,
            b: U8
        outs
            res: U8
        ;

    node zero
        type Op
        ins
        outs
            res: U8
        ;

    node sink
        type Op
        ins
            a: ?a
        outs
        ;

    node num
        type Op
        ins
        outs
            res: U8
        attrs
            num
        ;

}

#[test]
fn test_build_node() {
    let zero = mk!(arith.zero).unwrap();
    let one = mk!(arith.num, attr__num: Value::Number(1.into())).unwrap();
    let add = mk!(arith.add, a: zero.#res, b: one.#res).unwrap();
    let _sink = mk!(arith.sink, a: add.#res);

    // test if type implies work
    let res = one.out__res();
    let t = res.get_type().unwrap();
    // ty not yet denormed. match should denorm into new Type
    assert!(t.matches(&ty!(core.Clone)).unwrap());
    // denorm and check
    assert_eq!(t.denorm().unwrap().to_string(), "arith.U8 + core.Clone");

    assert!(cast!(arith.num, one.node()).is_some());
    assert!(cast!(arith.zero, one.node()).is_none());
    assert!(cast!(dyn arith.num, one.node()).is_some());
    assert!(cast!(dyn arith.zero, one.node()).is_none());
}
