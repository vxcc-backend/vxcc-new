use vxcc_dialect_parser::dialect;
use vxcc_ir::vxcc_core_dialect;

dialect! {
    dialect num: core, base;

    type U { width };
    type U { width: core.Lit } => core.Clone;
    type U { width: core.Lit } => core.Drop;

    type S { width };
    type S { width: core.Lit } => core.Clone;
    type S { width: core.Lit } => core.Drop;
}

#[cfg(test)]
mod tests {
    use super::*;
    use vxcc_type_parser_proc::ty;

    #[test]
    fn test_load() {
        let _ = ty!(num.U{width:8});
    }
}
