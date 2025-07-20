extern crate proc_macro;

use chumsky::{input::Stream, prelude::*};
use chumsky_proc_macro::*;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
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

fn dialect_parser<'src>() -> impl Parser<
    'src,
    chumsky::input::Stream<std::vec::IntoIter<TokenTreeWrapper>>,
    proc_macro2::TokenStream,
    chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>,
> {
    enum MemberSpec {
        TypeDecl {
            name: String,
            ground_ars: Option<Vec<String>>,
        },

        Implies {
            lhs: TokenStream,
            rhs: TokenStream,
        },

        Node {
            name: String,
            inputs: Vec<(String, TokenStream)>,
            outputs: Vec<(String, Option<TokenStream>)>,
            infer_func: Option<Ident>,
        },
    }

    // TODO: ensure no double underscores in idents

    let type_decl = exact_ident("type")
        .ignore_then(ident())
        .then(
            ident::<_, chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>>()
                .separated_by(punct(','))
                .at_least(1)
                .collect::<Vec<_>>()
                .grouped(GroupDelim::Brace)
                .or_not(),
        )
        .map(|(name, ground_ars)| MemberSpec::TypeDecl { name, ground_ars });

    let type_implies = exact_ident("type")
        .ignore_then(type_parser())
        .then_ignore(punct_seq("=>"))
        .then(type_parser())
        .map(|(lhs, rhs)| MemberSpec::Implies { lhs, rhs });

    let node_decl = exact_ident("node")
        .ignore_then(ident())
        .then_ignore(exact_ident("ins"))
        .then(
            ident()
                .then_ignore(punct(':'))
                .then(type_parser())
                .separated_by(punct(','))
                .collect::<Vec<(String, TokenStream)>>(),
        )
        .then_ignore(exact_ident("outs"))
        .then(
            ident()
                .then(punct(':').ignore_then(type_parser()).or_not())
                .separated_by(punct(','))
                .collect::<Vec<(String, Option<TokenStream>)>>(),
        )
        .then(
            exact_ident("infer")
                .ignore_then(punct('#'))
                .ignore_then(ident())
                .map(|x| Ident::new(x.as_str(), Span::call_site()))
                .or_not(),
        )
        .map(|(((name, inputs), outputs), infer_func)| MemberSpec::Node {
            name,
            inputs,
            outputs,
            infer_func,
        });

    let member_spec = choice((type_implies, type_decl, node_decl));

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
                  .separated_by(punct(','))
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
            let mut nodes_init_struct = TokenStream::new();
            let mut types_init_builder = TokenStream::new();
            let mut members_nodestruct = TokenStream::new();

            let members_typestruct = members
                .iter()
                .flat_map(|spec| match spec {
                    MemberSpec::Node { name, inputs, outputs, infer_func } => {
                        let name_id = Ident::new(name.as_str(), Span::call_site());

                        let gn = format!("DialectNode__{}", name);
                        let gn = Ident::new(gn.as_str(), Span::call_site());

                        let gn_ins = inputs
                            .iter()
                            .flat_map(|(k,_)| {
                                let kn = format!("in__{}", k);
                                let kn = Ident::new(kn.as_str(), Span::call_site());
                                quote! { pub #kn: ::vxcc_ir::NodePortVecIdx, }
                            })
                            .collect::<TokenStream>();

                        let gn_outs = outputs
                            .iter()
                            .flat_map(|(k,_)| {
                                let kn = format!("out__{}", k);
                                let kn = Ident::new(kn.as_str(), Span::call_site());
                                quote! { pub #kn: ::vxcc_ir::NodePortVecIdx, }
                            })
                            .collect::<TokenStream>();

                        builders = quote! {
                            #builders

                            pub struct #gn {
                                pub ty: ::vxcc_ir::NodeType,
                                #gn_ins
                                #gn_outs
                            }
                        };

                        let gnb = format!("DialectNodeBuilder__{}", name);
                        let gnb = Ident::new(gnb.as_str(), Span::call_site());

                        let gnb_ins = inputs
                            .iter()
                            .flat_map(|(k,_)| {
                                let kn = Ident::new(k.as_str(), Span::call_site());
                                quote! { pub #kn: ::vxcc_ir::Out, }
                            })
                            .collect::<TokenStream>();

                        let gna = format!("DialectNodeRef__{}", name);
                        let gna = Ident::new(gna.as_str(), Span::call_site());

                        let gnb_new_ins = inputs
                            .iter()
                            .flat_map(|(k,_)| {
                                let kn = Ident::new(k.as_str(), Span::call_site());
                                quote! { (#k,self.#kn), }
                            })
                            .collect::<TokenStream>();

                        builders = quote! {
                            #builders

                            pub struct #gnb {
                                #gnb_ins
                            }

                            impl #gnb {
                                pub fn build(self) -> #gna {
                                    let nd = ::vxcc_ir::Node::new(&DIALECT.nodes.#name_id.ty, [#gnb_new_ins].into_iter()).unwrap();
                                    #gna { nd }
                                }
                            }
                        };

                        let gna_ins = inputs.iter()
                            .flat_map(|(name, _)| {
                                let argid = format!("in__{}", name);
                                let argid = Ident::new(argid.as_str(), Span::call_site());

                                quote! {
                                    pub fn #argid(&self) -> ::vxcc_ir::In {
                                        unsafe {
                                            ::vxcc_ir::In::new(self.node(), DIALECT.nodes.#name_id.#argid)
                                        }
                                    }
                                }
                            })
                            .collect::<TokenStream>();

                        let gna_outs = outputs.iter()
                            .flat_map(|(name, _)| {
                                let argid = format!("out__{}", name);
                                let argid = Ident::new(argid.as_str(), Span::call_site());

                                quote! {
                                    pub fn #argid(&self) -> ::vxcc_ir::Out {
                                        unsafe {
                                            ::vxcc_ir::Out::new(self.node(), DIALECT.nodes.#name_id.#argid)
                                        }
                                    }
                                }
                            })
                            .collect::<TokenStream>();

                        builders = quote! {
                            #builders

                            pub struct #gna {
                                nd: ::vxcc_ir::Node,
                            }

                            impl #gna {
                                pub fn node(&self) -> ::vxcc_ir::Node {
                                    self.nd.clone()
                                }

                                #gna_ins
                                #gna_outs
                            }

                            impl TryFrom<::vxcc_ir::Node> for #gna {
                                type Error = ();

                                fn try_from(node: ::vxcc_ir::Node) -> Result<#gna, Self::Error> {
                                    if node.get_type() != DIALECT.nodes.#name_id.ty {
                                        Err(())
                                    } else {
                                        Ok(#gna { nd: node })
                                    }
                                }
                            }
                        };

                        members_nodestruct = quote! {
                            #members_nodestruct
                            pub #name_id: #gn,
                        };

                        let nd_name_id = format!("nd__{}", name_id);
                        let nd_name_id = Ident::new(nd_name_id.as_str(), Span::call_site());

                        let init_ins = inputs.iter()
                            .flat_map(|(name,ty)| quote! { (#name,#ty), })
                            .collect::<TokenStream>();

                        let init_outs = outputs.iter()
                            .flat_map(|(name,_)| quote! { #name, })
                            .collect::<TokenStream>();

                        let gn_infer = format!("DialectNodeInfer__{}", name);
                        let gn_infer = Ident::new(gn_infer.as_str(), Span::call_site());

                        types_init_builder = quote! {
                            #types_init_builder
                            let #nd_name_id = builder.add_node_type(#name,
                                                                        Box::new(#gn_infer {}),
                                                                        [#init_ins].into_iter(),
                                                                        [#init_outs].into_iter()).unwrap();
                        };

                        let initstruct_ins = inputs.iter()
                            .enumerate()
                            .flat_map(|(idx, (name,_))| {
                                let idx = idx as u8;
                                let argid = format!("in__{}", name);
                                let argid = Ident::new(argid.as_str(), Span::call_site());
                                quote! { #argid: #idx, }
                            })
                            .collect::<TokenStream>();

                        let initstruct_outs = outputs.iter()
                            .enumerate()
                            .flat_map(|(idx, (name,_))| {
                                let idx = idx as u8;
                                let argid = format!("out__{}", name);
                                let argid = Ident::new(argid.as_str(), Span::call_site());
                                quote! { #argid: #idx, }
                            })
                            .collect::<TokenStream>();

                        nodes_init_struct = quote! {
                            #nodes_init_struct
                            #name_id: #gn {
                                ty: #nd_name_id,
                                #initstruct_ins
                                #initstruct_outs
                            }
                        };

                        let infer_func_impl = match infer_func {
                            Some(func) => {
                                if outputs.iter().any(|(_,x)| x.is_some()) {
                                    panic!("automatic type inference in combination with manual inference function is currently not supported");
                                }
                                quote! { #func(node, out) }
                            }
                            None => {
                                let type_infers = outputs.iter()
                                    .map(|(name,ty)| {
                                        let ty = ty.as_ref().expect("no output type set. either set a output type, or use a manual inference function");
                                        quote! {
                                            out.set_name(#name, #ty)?;
                                        }
                                    })
                                    .collect::<proc_macro2::TokenStream>();

                                quote! {
                                    use super::#nmod as vxcc___dialect;
                                    #type_infers
                                    Ok(())
                                }
                            }
                        };

                        builders = quote! {
                            #builders

                            struct #gn_infer {}
                            impl ::vxcc_ir::NodeOutTypeInfer for #gn_infer {
                                fn infer_outputs(&self, node: ::vxcc_ir::Node, out: &mut ::vxcc_ir::NodeOutTypeInferRes) -> Result<(), ::vxcc_ir::IrError> {
                                    #infer_func_impl
                                }
                            }
                        };

                        TokenStream::new()
                    }

                    MemberSpec::TypeDecl { name, ground_ars } => {
                        let nameid = Ident::new(name.as_str(), Span::call_site());

                        match ground_ars.as_ref() {
                            Some(ground_ars) => {
                                let gn = format!("DialectType__{}", name);
                                let gn = Ident::new(gn.as_str(), Span::call_site());

                                let gnb = format!("DialectTypeBuilder__{}", name);
                                let gnb = Ident::new(gnb.as_str(), Span::call_site());

                                let tbn = format!("type__{}", name);
                                let tbn = Ident::new(tbn.as_str(), Span::call_site());

                                let mut args = TokenStream::new();
                                let mut gnb_args = TokenStream::new();
                                let mut gnb_init_args = TokenStream::new();
                                let mut inner_type_inits = TokenStream::new();
                                let mut inner_struct_inits = TokenStream::new();

                                for oarg in ground_ars {
                                    let oarg_fmt = Ident::new(oarg.as_str(), Span::call_site());

                                    let arg = format!("arg__{}__{}", name, oarg);
                                    let arg = Ident::new(arg.as_str(), Span::call_site());
                                    args = quote! {
                                        #args
                                        pub #arg: ::vxcc_ir::types::GroundArgRef,
                                    };

                                    gnb_args = quote! {
                                        #gnb_args
                                        pub #oarg_fmt: ::vxcc_ir::types::Type,
                                    };

                                    gnb_init_args = quote! {
                                        #gnb_init_args
                                        ( DIALECT.types.#nameid.#arg.clone() , self.#oarg_fmt ),
                                    };

                                    inner_type_inits = quote! {
                                        #inner_type_inits
                                        let #arg = #tbn.ground_arg(&#oarg).unwrap();
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

                                    pub struct #gnb {
                                        #gnb_args
                                    }

                                    impl #gnb {
                                        pub fn build(self) -> ::vxcc_ir::types::Type {
                                            unsafe {
                                                ::vxcc_ir::types::Type::ground_kv(
                                                    &DIALECT.types.#nameid.ty,
                                                    [#gnb_init_args].into_iter()
                                                    ).unwrap_unchecked()
                                            }
                                        }
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
                            builder.add_implies(#lhs, #rhs).unwrap();
                        };

                        TokenStream::new()
                    }


                }).collect::<TokenStream>();

            quote! {
                #[allow(non_snake_case, non_camel_case_types, dead_code, unused_variables)]
            mod #nmod {
                #builders

                pub struct DialectTypes {
                    #members_typestruct
                }

                pub struct DialectNodes {
                    #members_nodestruct
                }

                pub struct Dialect {
                    pub dialect: ::vxcc_ir::DialectRef,
                    pub deps: Vec<::vxcc_ir::DialectDep>,
                    pub types: DialectTypes,
                    pub nodes: DialectNodes,
                }

                fn create() -> Dialect {
                    use super::#nmod as vxcc___dialect;

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
                            #nodes_init_struct
                        }
                    }
                }

                pub static DIALECT: ::std::sync::LazyLock<Dialect> = ::std::sync::LazyLock::new(create);
            } }
        })
}

#[proc_macro]
pub fn dialect(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let v = dialect_parser()
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
