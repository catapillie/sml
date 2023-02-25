use std::ops::Not;

use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    self,
    ext::IdentExt,
    parse::{self, Parse, ParseStream, Parser},
    punctuated::Punctuated,
    token::Brace,
    Field, FieldsNamed, Ident, Lifetime, Token,
};

pub fn gen_tokens(input: TokenStream) -> syn::Result<TokenStream> {
    let input_tokens: Vec<InputToken> = Punctuated::<_, Token![;]>::parse_terminated
        .parse2(input)?
        .into_iter()
        .collect();

    let variants = input_tokens.iter().map(InputToken::gen_variant);
    let discrs = input_tokens.iter().map(|token| &token.name);

    let constructors = input_tokens.iter().map(InputToken::gen_constructor);

    let structs = input_tokens.iter().map(InputToken::gen_struct);
    let getters = input_tokens.iter().map(InputToken::gen_getters);

    let span_match_branches = input_tokens.iter().map(InputToken::gen_span_match_branch);
    let discr_match_branches = input_tokens.iter().map(InputToken::gen_discr_match_branch);

    Ok(quote! {
        #[derive(Debug, PartialEq)]
        pub enum Token<'a> {
            #(#variants),*
        }

        #[derive(Debug, PartialEq, Clone, Copy)]
        pub enum TokenDiscr {
            #(#discrs),*
        }

        impl<'a> Token<'a> {
            #(#constructors)*

            pub fn span(&self) -> Option<crate::lexing::TokenSpan> {
                match self {
                    #(#span_match_branches),*
                }
            }

            pub fn discr(&self) -> TokenDiscr {
                match self {
                    #(#discr_match_branches),*
                }
            }
        }

        #(#structs)*

        #(#getters)*
    })
}

struct InputToken {
    name: Ident,
    has_lifetime: bool,
    nospan: bool,
    fields: Option<FieldsNamed>,
}

impl InputToken {
    fn gen_lifetime(&self) -> Option<TokenStream> {
        self.has_lifetime.then_some(quote!(<'a>))
    }

    fn gen_type(&self) -> TokenStream {
        let name = &self.name;
        let lifetime = self.gen_lifetime();

        quote! {
            #name #lifetime
        }
    }

    fn gen_fields<'a, 'b, T: ToTokens, S: ToTokens>(
        &'a self,
        map_fn: impl Fn(&'a Field) -> T + 'a,
        span_fn: impl Fn() -> S,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        let fields = self
            .fields
            .iter()
            .flat_map(|fields| &fields.named)
            .map(move |field| {
                let field = map_fn(field);
                quote!(#field)
            });

        let span = self.nospan.not().then(move || {
            let span = span_fn();
            quote!(#span)
        });

        fields.chain(span)
    }

    fn gen_variant(&self) -> TokenStream {
        let name = &self.name;
        let ty = self.gen_type();

        quote! {
            #name(#ty)
        }
    }

    fn gen_constructor(&self) -> TokenStream {
        let name = &self.name;
        let fn_name: Ident = syn::parse_str(&self.name.to_string().to_case(Case::Snake)).unwrap();

        let args = self.gen_fields(
            |field| {
                let name = &field.ident;
                let ty = &field.ty;

                quote! {
                    #name: #ty
                }
            },
            || quote! { span: crate::lexing::TokenSpan },
        );

        let fields = self.gen_fields(|field| &field.ident, || quote! { span });

        quote! {
            pub fn #fn_name(#(#args),*) -> Self {
                Self::#name(#name {
                    #(#fields),*
                })
            }
        }
    }

    fn gen_struct(&self) -> TokenStream {
        let ty = self.gen_type();

        let fields = self.gen_fields(|field| field, || quote! { span: crate::lexing::TokenSpan });

        quote! {
            #[derive(Debug, PartialEq)]
            pub struct #ty {
                #(#fields),*
            }
        }
    }

    fn gen_getters(&self) -> TokenStream {
        let ty = self.gen_type();
        let lifetime = self.gen_lifetime();

        let getters = self.gen_fields(
            |field| {
                let name = &field.ident;
                let ty = &field.ty;

                quote! {
                    pub fn #name(&self) -> &#ty {
                        &self.#name
                    }
                }
            },
            || {
                quote! {
                    pub fn span(&self) -> &crate::lexing::TokenSpan {
                        &self.span
                    }
                }
            },
        );

        quote! {
            impl #lifetime #ty {
                #(#getters)*
            }
        }
    }

    fn gen_span_match_branch(&self) -> TokenStream {
        let get_span = if self.nospan {
            quote!(None)
        } else {
            quote!(Some(token.span))
        };

        let name = &self.name;

        quote! {
            Self::#name(token) => #get_span
        }
    }

    fn gen_discr_match_branch(&self) -> TokenStream {
        let name = &self.name;

        quote! {
            Self::#name(_) => TokenDiscr::#name
        }
    }
}

impl Parse for InputToken {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        Ok(Self {
            name: {
                let name: Ident = input.parse()?;
                if !name.to_string().is_case(Case::UpperCamel) {
                    return Err(syn::Error::new_spanned(
                        name,
                        "Expected token name to be in upper camel case.",
                    ));
                }
                name
            },
            has_lifetime: input.peek(Token![<]) && {
                input.parse::<Token![<]>().unwrap();
                input.parse::<Lifetime>()?;
                input.parse::<Token![>]>()?;
                true
            },
            nospan: input.peek(Ident::peek_any) && {
                let nospan: Ident = input.parse().unwrap();
                if nospan != "nospan" {
                    return Err(syn::Error::new_spanned(
                        nospan,
                        "Expected identifier `nospan`.",
                    ));
                }
                true
            },
            fields: if input.peek(Brace) {
                Some(input.parse()?)
            } else {
                None
            },
        })
    }
}
