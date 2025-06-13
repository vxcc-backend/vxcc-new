use vxcc_dialect_parser::*;

dialect! {
    dialect arith:
        core
        ;

    type U8;

    type Vec { elt };

    type Iter { elt };

    type Vec { elt: ?e } => Iter { elt: ?elt };

    /*
    node add
        ins
            a: U8,
            b: U8
        outs
            res: U8
        ;*/
}
