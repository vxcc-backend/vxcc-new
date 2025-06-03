use vxcc_dialect_parser::*;

dialect! {
    dialect arith:
        core
        math
        ;

    type U8;

    type U16;

    type Vec { elt };
}
