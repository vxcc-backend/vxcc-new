use vxcc_dialect_parser::*;
use vxcc_ir::vxcc_core_dialect;

dialect! {
    dialect arith:
        core
        ;

    type U8;
    type U8 => core.Clone;

    type Vec { elt };

    type Iter { elt };

    type Vec { elt: ?e } => Iter { elt: ?elt };

    node add
        ins
            a: U8,
            b: U8
        outs
            res: U8
        ;

    node zero
        ins
        outs
            res: U8
        ;

    node sink
        ins
            a: ?a
        outs
        ;

}

#[test]
fn test_build_node() {
    let zero = mk!(arith.zero).unwrap();
    let add = mk!(arith.add, a: zero.#res, b: zero.#res).unwrap();
    let _sink = mk!(arith.sink, a: add.#res);
}
