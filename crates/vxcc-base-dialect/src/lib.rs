use vxcc_dialect_parser::dialect;
use vxcc_ir::vxcc_core_dialect;

dialect! {
    dialect base: core;

    // type for nodes
    // Prefer using ordering flow for side effects instead of this type on nodes
    type Effect { e };

    // a source location
    type Loc;
    type Loc => core.Clone;
    type Loc => core.Drop;

    node source_loc
        type ?a
        ins
        outs
            out: Loc
        attrs
            path,
            line,
            column // can be Null
        ;

    node named_loc
        type ?a
        ins
        outs
            out: Loc
        attrs name
        ;

    node opaque_loc
        type ?a
        ins
        outs
            out: Loc
        ;

    node merged_loc
        type ?a
        ins
            l: Loc,
            r: Loc
        outs
            out: Loc
        attrs
            merger  // string of what merged the locs. can be Null
        ;
}

#[cfg(test)]
mod tests {
    use super::*;
    use vxcc_dialect_parser::mk;

    #[test]
    fn test_load() {
        let _ = mk!(base.opaque_loc).unwrap();
    }
}
