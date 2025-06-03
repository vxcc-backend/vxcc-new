extern crate proc_macro;

use chumsky::{prelude::*, input::Stream};
use chumsky_proc_macro::*;
use quote::quote;
use proc_macro2::{TokenStream, Ident, Span};
use vxcc_type_parser::*;

struct TokenStreamWrapper(TokenStream);

impl Default for TokenStreamWrapper {
    fn default() -> Self {
        Self(TokenStream::new())
    }
}

impl chumsky::container::Container<TokenStream> for TokenStreamWrapper {
    fn push(&mut self, item: TokenStream) {
        self.0.extend(item);
    }
}

impl quote::ToTokens for TokenStreamWrapper {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.0.clone());
    }
}

fn dialect_parser<'src>() -> impl Parser<'src, chumsky::input::Stream<std::vec::IntoIter<TokenTreeWrapper>>, proc_macro2::TokenStream, chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>>
{
    #[non_exhaustive]
    enum MemberSpec {
        TypeDecl {
            name: String,
            ground_ars: Option<Vec<String>>,
        },

        Implies {
            lhs: TokenStream,
            rhs: TokenStream,
        }
    }

    // TODO: ensure no double underscores in idents

    let type_decl = exact_ident("type")
        .ignore_then(ident())
        .then(ident::<_, chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>>()
              .separated_by(punct(','))
              .at_least(1)
              .collect::<Vec<_>>()
              .grouped(GroupDelim::Brace)
              .or_not())
        .map(|(name, ground_ars)| MemberSpec::TypeDecl {
            name,
            ground_ars
        });

    let type_implies = exact_ident("type")
        .ignore_then(type_parser())
        .then_ignore(punct_seq("=>"))
        .then(type_parser())
        .map(|(lhs, rhs)| MemberSpec::Implies {
            lhs,
            rhs,
        });

    let member_spec = choice((
        type_decl,
        type_implies,
    ));

    exact_ident("dialect")
        .ignore_then(ident())
        .then(punct(':')
            .ignore_then(punct('?')
                  .or_not()
                  .then(ident())
                  .map(|(o,n)| {
                      let o = o.is_some();
                      quote! { vxcc_ir::DialectDep { optional: #o, name: #n.to_string() } , }
                  })
                  .repeated()
                  .collect::<TokenStreamWrapper>())
            .or_not())
        .then_ignore(punct(';'))
        .then(member_spec
              .then_ignore(punct(';'))
              .repeated()
              .collect::<Vec<_>>())
        .map(|((n,deps), members)| {
            let deps = deps.map(|x| x.0).unwrap_or_else(|| quote! {});

            let nmod = format!("vxcc_{}_dialect", n);
            let nmod = Ident::new(nmod.as_str(), Span::call_site());

            let mut builders = TokenStream::new();
            let mut types_init_struct = TokenStream::new();
            let mut types_init_builder = TokenStream::new();

            let members_typestruct = members
                .iter()
                .flat_map(|spec| match spec {
                    MemberSpec::TypeDecl { name, ground_ars } => {
                        let nameid = Ident::new(name.as_str(), Span::call_site());

                        match ground_ars.as_ref() {
                            Some(ground_ars) => {
                                let gn = format!("DialectType__{}", name);
                                let gn = Ident::new(gn.as_str(), Span::call_site());

                                let tbn = format!("type__{}", name);
                                let tbn = Ident::new(tbn.as_str(), Span::call_site());

                                let mut args = TokenStream::new();
                                let mut inner_type_inits = TokenStream::new();
                                let mut inner_struct_inits = TokenStream::new();

                                for oarg in ground_ars {
                                    let arg = format!("arg__{}", oarg);
                                    let arg = Ident::new(arg.as_str(), Span::call_site());
                                    args = quote! {
                                        #args
                                        pub #arg: ::vxcc_ir::types::GroundArgRef,
                                    };

                                    inner_type_inits = quote! {
                                        #inner_type_inits
                                        let #arg = #tbn.ground_arg(#oarg).unwrap();
                                    };

                                    inner_struct_inits = quote! {
                                        #inner_struct_inits
                                        #arg,
                                    };
                                }

                                builders = quote! {
                                    #builders

                                    pub struct #gn {
                                        pub ty: ::vxcc_ir::types::TypeVar,
                                        #args
                                    }
                                };

                                let argsstr = ground_ars.iter()
                                    .flat_map(|a| quote! { #a, })
                                    .collect::<TokenStream>();

                                types_init_builder = quote! {
                                    #types_init_builder
                                    let #tbn = builder.add_ground_type(#name, [#argsstr].into_iter());
                                    #inner_type_inits
                                };

                                types_init_struct = quote! {
                                    #types_init_struct
                                    #nameid: #gn {
                                        ty: #tbn,
                                        #inner_struct_inits
                                    },
                                };

                                quote! {
                                    pub #nameid: #gn,
                                }
                            }

                            None => {
                                let tbn = format!("type__{}", name);
                                let tbn = Ident::new(tbn.as_str(), Span::call_site());

                                types_init_builder = quote! {
                                    #types_init_builder
                                    let #tbn = builder.add_type(#name);
                                };

                                types_init_struct = quote! {
                                    #types_init_struct
                                    #nameid: #tbn,
                                };

                                quote! {
                                    pub #nameid: ::vxcc_ir::types::TypeVar,
                                }
                            }
                        }
                    }

                    MemberSpec::Implies { lhs, rhs } => {
                        types_init_builder = quote! {
                            #types_init_builder
                            builder.add_implies(#lhs, #rhs);
                        };

                        TokenStream::new()
                    }
                }).collect::<TokenStream>();

            quote! { mod #nmod {
                #builders

                #[allow(non_snake_case)]
                pub struct DialectTypes {
                    #members_typestruct
                }

                #[allow(non_snake_case)]
                pub struct DialectNodes {

                }

                pub struct Dialect {
                    pub dialect: ::vxcc_ir::DialectRef,
                    pub deps: Vec<::vxcc_ir::DialectDep>,
                    pub types: DialectTypes,
                    pub nodes: DialectNodes,
                }

                #[allow(non_snake_case)]
                fn create() -> Dialect {
                    let mut builder = ::vxcc_ir::DialectBuilder::new(#n);

                    let Clone = builder.add_type("Clone");

                    #types_init_builder

                    let dialect = builder.build();
                    let dialect = dialect.get_dialect();

                    Dialect {
                        dialect,
                        deps: vec![ #deps ],
                        types: DialectTypes {
                            #types_init_struct
                        },
                        nodes: DialectNodes {

                        }
                    }
                }

                ::lazy_static::lazy_static! {
                    pub static ref DIALECT: Dialect = create();
                }
            } }
        })
}

#[proc_macro]
pub fn dialect(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let v = dialect_parser()
        .parse(Stream::from_iter(input.into_iter().map(|x| TokenTreeWrapper(x)).collect::<Vec<_>>().into_iter()))
        .into_result()
        .unwrap();

    v.into()
}
