use chumsky::prelude::*;
use chumsky_proc_macro::*;
use quote::quote;
use nostd::prelude::*;

pub fn type_parser<'src>() -> impl Parser<'src, chumsky::input::Stream<vec::IntoIter<TokenTreeWrapper>>, proc_macro2::TokenStream, chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>>
{
    enum Var {
        Dyn { ty: proc_macro2::TokenStream },
        Static {
            dialect: proc_macro2::TokenStream,
            name: proc_macro2::Ident,
        }
    }

    impl quote::ToTokens for Var {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            match self {
                Var::Dyn { ty } => tokens.extend(ty.clone()),
                Var::Static { dialect, name } => tokens.extend(quote! { #dialect::DIALECT.types.#name }),
            }
        }
    }

    recursive::<_, _, chumsky::extra::Err<Rich<'src, _>>, _, _>(|typ| {

        let id = ident()
            .then_ignore(punct('.'))
            .or_not()
            .map(|v| v.unwrap_or_else(|| "_".to_string()))
            .then(ident());

        let var = exact_ident("dyn")
            .ignore_then(id.clone())
            .map(|(d,v)| Var::Dyn { ty: quote! {
                ::vxcc_ir::DialectRegistry::get_dialect(#d).unwrap()
                    .find_type(#v).unwrap()
            } })
            .or(id.clone()
                .map(|(d,v)| {
                    let d = format!("vxcc_{}_dialect", d);
                    let d = proc_macro2::Ident::new(d.as_str(), proc_macro2::Span::call_site());
                    let v = proc_macro2::Ident::new(v.as_str(), proc_macro2::Span::call_site());
                    Var::Static {
                        dialect: quote! { #d },
                        name: v,
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

        let ground = var.clone()
            .then(ident()
                .then_ignore(punct(':'))
                .then(typ.clone())
                .separated_by(punct(','))
                .at_least(1)
                .collect::<Vec<(String, proc_macro2::TokenStream)>>()
                .grouped(GroupDelim::Brace))
            .map(|(var, args)| {
                match &var {
                    Var::Dyn { ty } => {
                        let args = args
                            .into_iter()
                            .flat_map(|(k,v)| quote! { (#var.ground_arg(#k).unwrap(),#v), })
                            .collect::<proc_macro2::TokenStream>();
                        quote! { ::vxcc_ir::types::Type::ground_kv(&#ty, [#args].into_iter()) }
                    }

                    Var::Static { dialect, name } => {
                        let inits = args
                            .into_iter()
                            .flat_map(|(k,v)| {
                                let kid = proc_macro2::Ident::new(k.as_str(), proc_macro2::Span::call_site());
                                quote! { #kid: #v, }
                            })
                            .collect::<proc_macro2::TokenStream>();

                        let gnb = format!("DialectTypeBuilder__{}", name.to_string());
                        let gnb = proc_macro2::Ident::new(gnb.as_str(), proc_macro2::Span::call_site());
                        quote! { #dialect::#gnb {
                            #inits
                        }.build() }
                    }
                }
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
