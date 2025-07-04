use vxcc_ir::vxcc_core_dialect;
use vxcc_type_parser_proc::*;

#[test]
fn a() {
    let _ = vxcc_core_dialect::DIALECT.dialect;

    let mut builder = vxcc_ir::DialectBuilder::new("test");
    let _ = builder.add_ground_type("Vector", ["elt", "v"].into_iter());
    let _ = builder.build();

    let _t = ty!(dyn core.Clone);
    let t = ty!(core.Clone);
    let _t = ty!(#t);
    let _t = ty!(?);
    let _t = ty!(?hey);
    let t = ty!(core.Clone + dyn core.Drop + ?yo);
    let _t = ty!(dyn test.Vector { elt: #t, v: core.Clone + core.Drop });
}
