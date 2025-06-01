extern crate proc_macro;

use chumsky::{prelude::*, input::Stream};
use chumsky_proc_macro::*;
use quote::quote;

fn type_parser<'src>() -> impl Parser<'src, chumsky::input::Stream<std::vec::IntoIter<TokenTreeWrapper>>, proc_macro2::TokenStream, chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>>
{
    recursive::<_, _, chumsky::extra::Err<Rich<'src, _>>, _, _>(|typ| {

        let id = ident()
            .then_ignore(punct('.'))
            .then(ident());

        let var = exact_ident("dyn")
            .ignore_then(id.clone())
            .map(|(d,v)| quote! {
                ::vxcc_ir::DialectRegistry::get_dialect(#d).unwrap()
                    .find_type(#v).unwrap()
            })
            .or(id.clone()
                .map(|(d,v)| {
                    let d = format!("vxcc_{}_dialect", d);
                    let d = proc_macro2::Ident::new(d.as_str(), proc_macro2::Span::call_site());
                    let v = proc_macro2::Ident::new(v.as_str(), proc_macro2::Span::call_site());
                    quote! {
                        #d::DIALECT.types.#v.clone()
                    }
                }));

        let qt = punct('#')
            .ignore_then(ident())
            .map(|v| {
                let v = proc_macro2::Ident::new(v.as_str(), proc_macro2::Span::call_site());
                quote! { ::vxcc_ir::types::Type::from(#v) }
            });

        let any = punct('?')
            .map(|_| quote! { ::vxcc_ir::types::Type::any() });

        let unspec = punct('?')
            .ignore_then(ident())
            .map(|i| quote! { ::vxcc_ir::types::Type::unspec(#i) });

        // TODO: fill members of init struct when not var dyn
        let ground = var.clone()
            .then(ident()
                .then_ignore(punct(':'))
                .then(typ.clone())
                .separated_by(punct(','))
                .at_least(1)
                .collect::<Vec<(String, proc_macro2::TokenStream)>>()
                .grouped(GroupDelim::Brace))
            .map(|(var, args)| {
                let args = args
                    .into_iter()
                    .flat_map(|(k,v)| quote! { (#var.ground_arg(#k).unwrap(),#v), })
                    .collect::<proc_macro2::TokenStream>();
                quote! { ::vxcc_ir::types::Type::ground_kv(&#var, [#args].into_iter()) }
            });

        let var_ty = var
            .map(|x| quote! { ::vxcc_ir::types::Type::var(&#x) });

        let atom: Boxed<'_, '_, _, proc_macro2::TokenStream, chumsky::extra::Err<Rich<'src, _>>> = choice((
            ground,
            var_ty,
            qt,
            unspec,
            any,
        )).boxed();

        let o: Boxed<'_, '_, _, proc_macro2::TokenStream, chumsky::extra::Err<Rich<'src, _>>> = atom.clone()
            .map(|x| quote! { #x , })
            .separated_by(punct('+'))
            .at_least(2)
            .collect::<Vec<_>>()
            .map(|x| {
                let x = x.into_iter().flatten().collect::<proc_macro2::TokenStream>();
                quote! { ::vxcc_ir::types::Type::and([#x].into_iter()).unwrap() }
            })
            .or(atom)
            .boxed();

        o
    })
}

#[proc_macro]
pub fn ty(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let v = type_parser()
        .parse(Stream::from_iter(input.into_iter().map(|x| TokenTreeWrapper(x)).collect::<Vec<_>>().into_iter()))
        .into_result()
        .unwrap();

    v.into()
}
