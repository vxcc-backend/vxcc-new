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

impl chumsky::container::Container<TokenTreeWrapper> for TokenStreamWrapper {
    fn push(&mut self, item: TokenTreeWrapper) {
        self.0.extend([item.0].into_iter());
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
            ty: TokenStream,
            inputs: Vec<(String, TokenStream)>,
            outputs: Vec<(String, Option<TokenStream>)>,
            attrs: Vec<String>,
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
        .ignore_then(type_parser(false))
        .then_ignore(punct_seq("=>"))
        .then(type_parser(false))
        .map(|(lhs, rhs)| MemberSpec::Implies { lhs, rhs });

    let node_decl = exact_ident("node")
        .ignore_then(ident())
        .then_ignore(exact_ident("type"))
        .then(type_parser(false))
        .then_ignore(exact_ident("ins"))
        .then(
            ident()
                .then_ignore(punct(':'))
                .then(type_parser(false))
                .separated_by(punct(','))
                .collect::<Vec<(String, TokenStream)>>(),
        )
        .then_ignore(exact_ident("outs"))
        .then(
            ident()
                .then(punct(':').ignore_then(type_parser(false)).or_not())
                .separated_by(punct(','))
                .collect::<Vec<(String, Option<TokenStream>)>>(),
        )
        .then(
            exact_ident("attrs")
                .ignore_then(ident().separated_by(punct(',')).collect::<Vec<String>>())
                .or_not()
                .map(|x| x.unwrap_or(vec![])),
        )
        .then(
            exact_ident("infer")
                .ignore_then(punct('#'))
                .ignore_then(ident())
                .map(|x| Ident::new(x.as_str(), Span::call_site()))
                .or_not(),
        )
        .map(
            |(((((name, ty), inputs), outputs), attrs), infer_func)| MemberSpec::Node {
                name,
                ty,
                inputs,
                outputs,
                attrs,
                infer_func,
            },
        );

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
            let mut nodes_init_builder = TokenStream::new();
            let mut members_nodestruct = TokenStream::new();

            let members_typestruct = members
                .into_iter()
                .flat_map(|spec| match spec {
                    MemberSpec::Node { name, ty, inputs, outputs, attrs, infer_func } => {
                        let mut outputs = outputs;
                        outputs.push(("_attach".to_string(), Some(quote! { ::vxcc_ir::types::Type::var(&vxcc_core_dialect::DIALECT.types.Attach) })));
                        let outputs = outputs;

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

                        let gn_attrs = attrs
                            .iter()
                            .flat_map(|k| {
                                let kn = format!("attr__{}", k);
                                let kn = Ident::new(kn.as_str(), Span::call_site());
                                quote! { pub #kn: u8, }
                            })
                            .collect::<TokenStream>();

                        builders = quote! {
                            #builders

                            pub struct #gn {
                                pub ty: ::vxcc_ir::NodeType,
                                #gn_ins
                                #gn_outs
                                #gn_attrs
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

                        let gnb_attrs = attrs
                            .iter()
                            .flat_map(|k| {
                                let kn = Ident::new(format!("attr__{}", k).as_str(), Span::call_site());
                                quote! { pub #kn: ::vxcc_ir::Value, }
                            })
                            .collect::<TokenStream>();

                        let gna = format!("DialectNodeRef__{}", name);
                        let gna = Ident::new(gna.as_str(), Span::call_site());

                        let gnb_new_ins_iter = if inputs.len() == 0 {
                            quote! { ::std::iter::empty::<(&'static str, ::vxcc_ir::Out)>() }
                        } else {
                            let gnb_new_ins = inputs
                                .iter()
                                .flat_map(|(k,_)| {
                                    let kn = Ident::new(k.as_str(), Span::call_site());
                                    quote! { (#k,self.#kn), }
                                })
                                .collect::<TokenStream>();

                            quote! {
                                [#gnb_new_ins].into_iter()
                            }
                        };

                        let gnb_new_attrs_iter = if attrs.len() == 0 {
                            quote! { ::std::iter::empty::<(&'static str, ::vxcc_ir::Value)>() }
                        } else {
                            let a = attrs
                                .iter()
                                .flat_map(|k| {
                                    let kn = Ident::new(format!("attr__{}", k).as_str(), Span::call_site());
                                    quote! { (#k,self.#kn), }
                                })
                                .collect::<TokenStream>();

                            quote! {
                                [#a].into_iter();
                            }
                        };

                        builders = quote! {
                            #builders

                            pub struct #gnb {
                                #gnb_ins
                                #gnb_attrs
                            }

                            impl #gnb {
                                pub fn build(self) -> Result<#gna, ::vxcc_ir::IrError> {
                                    let ins = #gnb_new_ins_iter;
                                    let attrs = #gnb_new_attrs_iter;
                                    let nd = ::vxcc_ir::Node::new(&DIALECT.nodes.#name_id.ty.clone(), ins, attrs)?;
                                    Ok(#gna { nd })
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

                        let gna_attrs = attrs.iter()
                            .flat_map(|name| {
                                let argid = format!("attr__{}", name);
                                let argid = Ident::new(argid.as_str(), Span::call_site());

                                quote! {
                                    pub fn #argid(&self) -> ::vxcc_ir::Value {
                                        self.node().attr(#name).unwrap()
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
                                #gna_attrs
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

                        let init_ins = if inputs.len() == 0 {
                            quote! { ::std::iter::empty::<(&'static str, ::vxcc_ir::types::Type)>() }
                        } else {
                            let i = inputs.iter()
                                .flat_map(|(name,ty)| quote! { (#name,#ty), })
                                .collect::<TokenStream>();
                            quote! { [#i].into_iter() }
                        };

                        let init_outs = if outputs.len() == 0 {
                            quote! { ::std::iter::empty::<&'static str>() }
                        } else {
                            let i = outputs.iter()
                                .flat_map(|(name,_)| quote! { #name, })
                                .collect::<TokenStream>();
                            quote! { [#i].into_iter() }
                        };

                        let init_attrs = if attrs.len() == 0 {
                            quote! { ::std::iter::empty::<&'static str>() }
                        } else {
                            let i = attrs.iter()
                                .flat_map(|k| quote! { #k, })
                                .collect::<TokenStream>();
                            quote! { [#i].into_iter() }
                        };

                        let gn_infer = format!("DialectNodeInfer__{}", name);
                        let gn_infer = Ident::new(gn_infer.as_str(), Span::call_site());

                        nodes_init_builder = quote! {
                            #nodes_init_builder
                            let #nd_name_id = builder.add_node_type(#name, #ty,
                                                                        Box::new(#gn_infer {}),
                                                                        #init_ins,
                                                                        #init_outs,
                                                                        #init_attrs).unwrap();
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

                        let initstruct_attrs = attrs.iter()
                            .enumerate()
                            .flat_map(|(idx, name)| {
                                let idx = idx as u8;
                                let argid = format!("attr__{}", name);
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
                                #initstruct_attrs
                            },
                        };

                        let mut type_infers = outputs.iter()
                            .filter_map(|(name,ty)| {
                                let ty = match (ty.as_ref(), &infer_func) {
                                    (Some(x), _) => Some(x),
                                    (None, None) => panic!("no output type set. either set a output type, or use a manual inference function"),
                                    (None, Some(_)) => None,
                                };

                                ty.map(|ty| quote! { out.set_name(#name, #ty)?; })
                            })
                            .collect::<proc_macro2::TokenStream>();

                        if let Some(func) = infer_func {
                            type_infers = quote! {
                                #type_infers
                                #func(node, out)
                            };
                        };

                        let infer_func_impl = quote! {
                            use super::#nmod as vxcc___dialect;
                            use super::*;
                            #type_infers
                            Ok(())
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
                                                    &DIALECT.types.#nameid.ty.clone(),
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
                        nodes_init_builder = quote! {
                            #nodes_init_builder
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
                    wip_builder: ::std::sync::Mutex<::std::cell::RefCell<Option<::vxcc_ir::DialectBuilder>>>,
                    pub nodes: ::std::sync::LazyLock<DialectNodes>,
                }

                fn create_nodes() -> DialectNodes {
                    use super::#nmod as vxcc___dialect;
                    use super::*;

                    let mut builder = DIALECT.wip_builder.lock().unwrap().replace(None).unwrap();

                    #nodes_init_builder

                    let dialect = builder.build();

                    DialectNodes {
                        #nodes_init_struct
                    }
                }

                fn lateinit() {
                    let _ = &DIALECT.nodes;
                }

                fn create() -> Dialect {
                    use super::#nmod as vxcc___dialect;
                    use super::*;

                    let mut builder = ::vxcc_ir::DialectBuilder::new(#n);
                    builder.dont_call_this_add_lateinit(lateinit);

                    #types_init_builder

                    Dialect {
                        dialect: builder.wip_ref(),
                        deps: vec![ #deps ],
                        types: DialectTypes {
                            #types_init_struct
                        },
                        wip_builder: ::std::sync::Mutex::new(::std::cell::RefCell::new(Some(builder))),
                        nodes: ::std::sync::LazyLock::new(create_nodes),
                    }
                }

                pub static DIALECT: ::std::sync::LazyLock<Dialect> = ::std::sync::LazyLock::new(create);
            } }
        })
}

/// create a vxcc dialect
///
/// example:
/// ```
/// fn my_infer_func(node: vxcc_ir::Node, out: &mut vxcc_ir::NodeOutTypeInferRes) -> Result<(), vxcc_ir::IrError> {
///    ...
/// }
///
/// dialect! {
///     dialect arith:
///         core
///         ;
///
///     type U8;
///     type U8 => core.Clone;
///
///     type Vec { elt };
///
///     type Iter { elt };
///
///     type Vec { elt: ?e } => Iter { elt: ?elt };
///
///     type Op;
///
///     node add
///         type Op
///         ins
///             a: U8,
///             b: U8
///         outs
///             res: U8
///         ;
///
///     node other
///         type Op
///         ins
///         outs
///             res: U8
///         attrs
///             attr1, attr2
///         infer #my_infer_func
///         ;
/// }
/// ```
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

fn mk_parser<'src>() -> impl Parser<
    'src,
    chumsky::input::Stream<std::vec::IntoIter<TokenTreeWrapper>>,
    proc_macro2::TokenStream,
    chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>,
> {
    vxcc_type_parser::var_parser()
        .then(
            punct(',')
                .ignore_then(
                    any()
                        .repeated()
                        .collect::<TokenStreamWrapper>()
                        .map(|x| x.0),
                )
                .or_not(),
        )
        .map(|(var, init)| {
            let init = init.map(|init| {
                init.into_iter()
                    .scan(false, |state, x| match (*state, x) {
                        (_, proc_macro2::TokenTree::Punct(punct)) if punct.as_char() == '#' => {
                            *state = true;
                            Some(vec![])
                        }
                        (false, x) => Some(vec![x]),
                        (true, proc_macro2::TokenTree::Ident(id)) => {
                            let id = format!("out__{}", id);
                            let id = Ident::new(id.as_str(), Span::call_site());
                            *state = false;
                            Some(quote! { #id() }.into_iter().collect())
                        }
                        (true, _) => panic!("no"),
                    })
                    .flatten()
                    .collect::<proc_macro2::TokenStream>()
            });

            match var {
                Var::Dyn { .. } => {
                    panic!("`mk!()` with dyn node type not yet implemented")
                }
                Var::Static { dialect, name } => {
                    let gnb = format!("DialectNodeBuilder__{}", name);
                    let gnb = Ident::new(gnb.as_str(), Span::call_site());
                    quote! {
                        #dialect::#gnb { #init }.build()
                    }
                }
            }
        })
}

/// build a node
///
/// example:
/// ```
/// let nd = mk!(some.Node, a: a, b: b).node();
/// ```
#[proc_macro]
pub fn mk(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let v = mk_parser()
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

fn cast_parser<'src>() -> impl Parser<
    'src,
    chumsky::input::Stream<std::vec::IntoIter<TokenTreeWrapper>>,
    proc_macro2::TokenStream,
    chumsky::extra::Err<Rich<'src, TokenTreeWrapper>>,
> {
    vxcc_type_parser::var_parser()
        .then(
            punct(',')
                .ignore_then(
                    any()
                        .repeated()
                        .collect::<TokenStreamWrapper>()
                        .map(|x| x.0),
                )
                .or_not(),
        )
        .map(|(var, init)| match var {
            Var::Dyn { dialect, name } => {
                quote! {
                    (#init).dyn_cast(#dialect, #name)
                }
            }
            Var::Static { dialect, name } => {
                let gnb = format!("DialectNodeRef__{}", name);
                let gnb = Ident::new(gnb.as_str(), Span::call_site());
                quote! {
                    TryInto::<#dialect::#gnb>::try_into((#init).clone()).ok()
                }
            }
        })
}

/// try cast a node to a typed node
///
/// example:
/// ```
/// let expr: &Node = ...;
/// let nd: Option<vxcc_some_dialect::DialectNodeRef__Node> = cast!(some.Node, expr);
/// ```
///
/// it can also be used to just check if a node is of a type, by using `dyn` in the node type:
/// ```
/// let expr: &Node = ...;
/// let nd: Option<Node> = cast!(dyn some.Node, expr);
/// ```
#[proc_macro]
pub fn cast(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let v = cast_parser()
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
