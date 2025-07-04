extern crate proc_macro;

use chumsky::{input::Stream, prelude::*};
use chumsky_proc_macro::*;
use vxcc_type_parser::*;

#[proc_macro]
pub fn ty(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let v = type_parser()
        .parse(Stream::from_iter(
            input
                .into_iter()
                .map(|x| TokenTreeWrapper(x))
                .collect::<Vec<_>>()
                .into_iter(),
        ))
        .into_result()
        .unwrap();

    v.into()
}
