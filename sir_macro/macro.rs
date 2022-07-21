extern crate proc_macro;
extern crate syn;
extern crate quote;
extern crate proc_macro2;

//use proc_macro::TokenStream;
use proc_macro2::TokenStream;
use syn::*;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use quote::{quote, ToTokens};

// Foo
struct Guard {
    event: Expr,
    // Foo => || Bar
    //handle: Option<Expr>,
}
struct Guards {
    _where: Token![where],
    _brace: token::Brace,
    guards: Punctuated<Guard, Token![,]>,
}
/*struct Init {
    _eq: Token![=],
    expr: Expr,
    // Just need [^,{where}]*
}*/
struct Name {
    name: Ident,
    _col: Token![:],
}
struct Field {
    name: Option<Name>,
    ty: Type,
    //init: Option<Init>,
    guards: Option<Guards>,
}
struct Inner {
    paren: Option<token::Paren>,
    brace: Option<token::Brace>,
    fields: Punctuated<Field, Token![,]>,
}
struct Variant {
    unit: Path,
    inner: Option<Inner>,
    guards: Option<Guards>,
}
enum Blade {
    Enum {
        _enum: Token![enum],
        name: Path,
        guards: Guards,
        _com: Token![,],
        variants: Punctuated<Variant, Token![,]>,
    },
    Struct {
        _struct: Token![struct],
        name: Path,
        guards: Guards,
        _com: Token![,],
        fields: Punctuated<Field, Token![,]>,
    }
}


#[proc_macro]
pub fn blade_impl(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let blade = syn::parse_macro_input!(tokens as Blade);
    let this = match &blade {
        Blade::Enum { name, .. } | Blade::Struct { name, .. } => name,
    };
    let body_out;
    let mut variants_out = quote![];
    match &blade {
        Blade::Enum { variants, ..  } => {
            let mut discrim_out = quote![];
            for (discrim_id, variant) in variants.iter().enumerate() {
                let guards = collect_guards(variant.guards.as_ref(), &quote![rt::Variant]);
                let unit = &variant.unit;
                let unit_str = &unit.segments.last().unwrap().ident;
                let unit_str = format!("{}", unit_str);
                let mut fields_out = quote![];
                let body_type;
                let variant_init;
                match &variant.inner {
                    None => {
                        body_type = quote![Unit];
                        discrim_out = quote! {
                            #discrim_out
                            #this :: #unit => #discrim_id,
                        };
                        variant_init = quote! { #this :: #unit };
                    },
                    Some(inner) => {
                        let Inner { fields, .. } = inner;
                        if let Inner { paren: Some(_), .. } = inner {
                            body_type = quote![Tuple];
                            let field_ty = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();
                            discrim_out = quote! {
                                #discrim_out
                                #this :: #unit(..) => #discrim_id,
                            };
                            variant_init = quote! {
                                #this :: #unit(#({
                                    let mut f = Option::<#field_ty>::None;
                                    _each(&mut f);
                                    if let Some(f) = f { f } else { return }
                                }),*)
                            };
                            for (i, field) in fields.iter().enumerate() {
                                let guards = collect_guards(field.guards.as_ref(), &quote![rt::Field]);
                                let pick = (0..fields.len())
                                    .map(|j| {
                                        if i == j {
                                            quote! { pick }
                                        } else {
                                            quote! { _ }
                                        }
                                    }).collect::<Vec<_>>();
                                let pick = quote! { #(#pick,)* };
                                let field_name = format!("{}", i);
                                let ty = field.ty.to_token_stream();
                                fields_out = quote! {
                                    #fields_out
                                    Arc::new(Field {
                                        name: #field_name,
                                        ty: Ty::of::<#ty>(),
                                        as_ref: |s: &dyn AnyDebug| -> &dyn AnyDebug {
                                            if let #this :: #unit(#pick) = Self::downcast_ref(s) {
                                                pick
                                            } else {
                                                variant_mismatch()
                                            }
                                        },
                                        as_mut: |s: &mut dyn AnyDebug| -> &mut dyn AnyDebug {
                                            if let #this :: #unit(#pick) = Self::downcast_mut(s) {
                                                pick
                                            } else {
                                                variant_mismatch()
                                            }
                                        },
                                        with: |f: &mut dyn FnMut(AnyOptionT)| f(&mut Option::<Self>::None),
                                        guards: #guards,
                                    }),
                                };
                            }
                        } else if let Inner { brace: Some(_), .. } = inner {
                            body_type = quote![Struct];
                            discrim_out = quote! {
                                #discrim_out
                                #this :: #unit { .. } => #discrim_id,
                            };
                            {
                                let field = fields.iter().map(|field| &field.name.as_ref().unwrap().name);
                                let field_ty = fields.iter().map(|field| field.ty.to_token_stream());
                                variant_init = quote! {
                                    #this :: #unit {
                                        #(#field: {
                                            let mut f = Option::<#field_ty>::None;
                                            _each(&mut f);
                                            if let Some(f) = f { f } else { return }
                                        }),*
                                    }
                                };
                            }
                            for field in fields {
                                let guards = collect_guards(field.guards.as_ref(), &quote![rt::Field]);
                                let pick = &field.name.as_ref().unwrap().name;
                                let field_name = format!("{}", pick);
                                let ty = field.ty.to_token_stream();
                                fields_out = quote! {
                                    #fields_out
                                    Arc::new(Field {
                                        name: #field_name,
                                        ty: Ty::of::<#ty>(),
                                        as_ref: |s: &dyn AnyDebug| -> &dyn AnyDebug {
                                            if let #this :: #unit { #pick, .. } = Self::downcast_ref(s) {
                                                #pick
                                            } else {
                                                variant_mismatch()
                                            }
                                        },
                                        as_mut: |s: &mut dyn AnyDebug| -> &mut dyn AnyDebug {
                                            if let #this :: #unit { #pick, .. } = Self::downcast_mut(s) {
                                                #pick
                                            } else {
                                                variant_mismatch()
                                            }
                                        },
                                        with: |f: &mut dyn FnMut(AnyOptionT)| f(&mut Option::<Self>::None),
                                        guards: #guards,
                                    }),
                                };
                            }
                        } else {
                            unreachable!("{}", "enum variant should be Foo, Foo(), or Foo{}")
                        }
                    },
                }
                variants_out = quote! {
                    #variants_out
                    Arc::new(Variant {
                        name: #unit_str,
                        body_type: BodyType::#body_type,
                        fields: vec![#fields_out],
                        init: |out: AnyOptionT, _each: &mut dyn FnMut(AnyOptionT)| {
                            let out: &mut Option<Self> = out.downcast_mut().expect("wrong type (enum)");
                            *out = Some(#variant_init);
                        },
                        guards: #guards,
                    }),
                };
            }
            body_out = quote! {
                Body::Enum(Arc::new(BodyEnum {
                    variants: vec![#variants_out],
                    variant_index: |s: &dyn AnyDebug| -> usize {
                        match Self::downcast_ref(s) {
                            #discrim_out
                        }
                    },
                }))
            };
        },
        Blade::Struct { fields, ..  } => {
            let mut body = quote![];
            let body_type = {
                let mut found = None;
                for field in fields {
                    let actual = Some(if field.name.is_none() {
                        BodyType::Tuple
                    } else {
                        BodyType::Struct
                    });
                    if found.is_none() {
                        found = actual
                    } else if found != actual {
                        let span = field.ty.span();
                        let msg = format!("all fields in struct must be either named or not named");
                        return Error::new(span, msg).into_compile_error().into();
                    }
                }
                found.unwrap_or(BodyType::Unit)
            };
            for (i, field) in fields.iter().enumerate() {
                let (field_name, str_name) = if let Some(name) = &field.name {
                    let name = name.name.clone();
                    let str_name = format!("{}", &name);
                    (syn::Member::Named(name), quote! { #str_name })
                } else {
                    let str_i = format!("{}", i);
                    (syn::Member::Unnamed(syn::Index {
                        index: i as u32,
                        span: field.ty.span(),
                    }), quote! { #str_i })
                };
                let guards = collect_guards(field.guards.as_ref(), &quote![rt::Struct]);
                let ty = field.ty.to_token_stream();
                body = quote! {
                    #body
                    Arc::new(Field {
                        name: #str_name,
                        ty: Ty::of::<#ty>(),
                        as_ref: |s: &dyn AnyDebug| -> &dyn AnyDebug {
                            &Self::downcast_ref(s).#field_name
                        },
                        as_mut: |s: &mut dyn AnyDebug| -> &mut dyn AnyDebug {
                            &mut Self::downcast_mut(s).#field_name
                        },
                        with: |f: &mut dyn FnMut(AnyOptionT)| f(&mut Option::<Self>::None),
                        guards: #guards,
                    }),
                };
            }
            let body_type = match body_type {
                BodyType::Unit => quote! { Unit },
                BodyType::Tuple => quote! { Tuple },
                BodyType::Struct => quote! { Struct },
            };
            let field_name = fields.iter().map(|field| &field.name.as_ref().unwrap().name);
            let field_type = fields.iter().map(|field| field.ty.to_token_stream());
            body_out = quote! {
                Body::Struct(Arc::new(BodyStruct {
                    body_type: BodyType::#body_type,
                    fields: vec![#body],
                    init: |out: AnyOptionT, each: &mut dyn FnMut(AnyOptionT)| {
                        let out: &mut Option<Self> = out.downcast_mut().unwrap();
                        let this = Self {#(
                            #field_name: {
                                let mut v = Option::<#field_type>::None;
                                each(&mut v);
                                if let Some(v) = v { v } else { return }
                            },
                        )*};
                        *out = Some(this);
                    },
                }))
            };
        },
    };
    let item_guards = match &blade {
        Blade::Enum { guards, .. } | Blade::Struct { guards, .. } => {
            collect_guards(Some(guards), &quote![rt::Item])
        },
    };
    let tokens = quote::quote! {{
        use crate::sir::prelude_macro::*;
        Sword {
            item: Arc::new(Item {
                ty: Ty::of::<#this>(),
                guards: #item_guards,
                body: #body_out,
            }),
        }
    }};

    if option_env!("LOG_BLADE_IMPL_MACRO").is_some() {
        use std::process::*;
        use std::io::Write;
        use std::fs::File;
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        let n = COUNT.fetch_add(1, Ordering::SeqCst);
        let path = format!("/tmp/blade_impl_{}.rs", n);
        let path: &str = &path;
        if let Ok(mut o) = File::create(path) {
            if write!(o, "fn deleteme() {{ {} }}", tokens).is_ok() {
                Command::new("rustfmt")
                    .arg(path)
                    .status()
                    .expect("rustfmt failed");
                use std::io::BufRead;
                let back = std::io::BufReader::new(File::open(path).unwrap());
                let mut lines = back.lines().map(std::result::Result::unwrap).collect::<Vec<String>>();
                let com = |s: &mut String| {
                    let f = format!("// {}", s);
                    *s = f;
                };
                com(&mut lines[0]);
                let n = lines.len() - 1;
                com(&mut lines[n]);
                let mut back = File::create(path).unwrap();
                for line in lines {
                    writeln!(back, "{}", line).ok();
                }
                return quote![include! { #path }].into();
            }
        }
    }
    proc_macro::TokenStream::from(tokens)
}

#[derive(PartialEq)]
enum BodyType {
    Unit,
    Tuple,
    Struct,
}


impl Parse for Guard {
    fn parse(input: ParseStream) -> Result<Self> {
        let event: Expr = input.parse()?;
        /*let handle = if input.peek(Token![=>]) {
            let _: Token![=>] = input.parse()?;
            let handle: Expr = input.parse()?;
            Some(handle)
        } else {
            None
        };*/
        Ok(Guard { event })
    }
}

impl Parse for Guards {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Guards {
            _where: input.parse()?,
            _brace: braced![content in input],
            guards: content.parse_terminated(Guard::parse)?,
        })
    }
}
fn collect_guards(guards: Option<&Guards>, position: &TokenStream) -> TokenStream {
    let mut out = quote! {};
    if let Some(guards) = guards {
        for guard in &guards.guards {
            let event = &guard.event;
            out = quote! {
                #out
                Guard::new::<#position, _>(#event),
            };
        }
    }
    let out = quote![vec![#out]];
    out.into()
}

/*impl Parse for Init {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Init {
            _eq: input.parse()?,
            expr: input.parse()?,
        })
    }
}*/

impl Parse for Name {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Name {
            name: input.parse()?,
            _col: input.parse()?,
        })
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = if input.peek2(Token![:]) {
            Some(input.parse()?)
        } else {
            None
        };
        let ty = input.parse()?;
        /*let init = if input.peek(Token![=]) {
            Some(input.parse()?)
        } else {
            None
        };*/
        let guards = if input.peek(Token![where]) {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(Field { name, ty, guards })
    }
}

impl Parse for Inner {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.lookahead1();
        let mut paren = None;
        let mut brace = None;
        let content;
        if ahead.peek(token::Paren) {
            paren = Some(parenthesized![content in input]);
        } else if ahead.peek(token::Brace) {
            brace = Some(braced![content in input]);
        } else {
            return Err(ahead.error());
        }
        let fields = content.parse_terminated(Field::parse)?;
        Ok(Inner { paren, brace, fields })
    }
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> Result<Self> {
        let unit = input.parse()?;
        let inner = if input.peek(token::Paren) || input.peek(token::Brace) {
            Some(input.parse()?)
        } else {
            None
        };
        let guards = if input.peek(Token![where]) {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(Variant { unit, inner, guards })
    }
}

impl Parse for Blade {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.lookahead1();
        Ok(if ahead.peek(Token![enum]) {
            let _enum = input.parse()?;
            let name = input.parse()?;
            let guards = input.parse()?;
            let _com = input.parse()?;
            let variants = input.parse_terminated(Variant::parse)?;
            Blade::Enum { _enum, name, guards, _com, variants }
        } else if ahead.peek(Token![struct]) {
            let _struct = input.parse()?;
            let name = input.parse()?;
            let guards = input.parse()?;
            let _com = input.parse()?;
            let fields = input.parse_terminated(Field::parse)?;
            Blade::Struct { _struct, name, guards, _com, fields }
        } else {
            return Err(ahead.error());
        })
    }
}
