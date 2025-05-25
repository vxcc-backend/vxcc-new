use crate::*;

pub struct CoreDialect {
    pub dialect: DialectRef,

    pub clone: types::TypeVar,
    pub drop: types::TypeVar,
}

fn create() -> CoreDialect {
    let mut builder = DialectBuilder::new("core");

    let clone = builder.add_type("Clone");
    let drop = builder.add_type("Drop");

    let own = builder.build();
    CoreDialect {
        dialect: own.get_dialect(),
        clone, drop
    }
}

lazy_static::lazy_static! {
    pub static ref DIALECT: CoreDialect = create();
}
