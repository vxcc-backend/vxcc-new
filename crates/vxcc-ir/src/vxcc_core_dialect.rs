use crate::*;

#[allow(non_snake_case)]
pub struct CoreTypes {
    pub Clone: types::TypeVar,
    pub Drop: types::TypeVar,
}

pub struct CoreDialect {
    pub dialect: DialectRef,
    pub types: CoreTypes,
}

#[allow(non_snake_case)]
fn create() -> CoreDialect {
    let mut builder = DialectBuilder::new("core");

    let Clone = builder.add_type("Clone");
    let Drop = builder.add_type("Drop");

    let own = builder.build();
    CoreDialect {
        dialect: own.get_dialect(),
        types: CoreTypes {
            Clone, Drop
        }
    }
}

pub static DIALECT: LazyLock<CoreDialect> = LazyLock::new(create);
