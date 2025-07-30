use crate::{types::Type, *};

#[allow(non_snake_case)]
pub struct CoreTypes {
    pub Clone: types::TypeVar,
    pub Drop: types::TypeVar,
    pub Attach: types::TypeVar,
    pub Lit: types::TypeVar,
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
    let Attach = builder.add_type("Attach");
    let Lit = builder.add_type("Lit");

    builder
        .add_implies(Type::var(&Attach), Type::var(&Clone))
        .unwrap();

    builder
        .add_implies(Type::var(&Attach), Type::var(&Drop))
        .unwrap();

    let own = builder.build();
    CoreDialect {
        dialect: own.get_dialect(),
        types: CoreTypes {
            Clone,
            Drop,
            Attach,
            Lit,
        },
    }
}

pub static DIALECT: LazyLock<CoreDialect> = LazyLock::new(create);
