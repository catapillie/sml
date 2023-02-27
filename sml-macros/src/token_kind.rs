use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    self, braced,
    parse::{self, Parse, ParseStream, Parser},
    Ident, Lifetime, Token,
};

pub fn gen_token_kind(input: TokenStream) -> syn::Result<TokenStream> {
    let (name, variants) = parse_input.parse2(input)?;

    let ty = if variants.iter().any(|variant| variant.has_lifetime) {
        quote!(#name<'a>)
    } else {
        quote!(#name)
    };

    let enum_variants = variants.iter().map(Variant::gen_enum_variant);
    let try_from_token_match_branches = variants
        .iter()
        .map(Variant::gen_try_from_token_match_branch);
    let into_token_match_branches = variants.iter().map(Variant::gen_into_token_match_branch);

    Ok(quote! {
        #[derive(Debug)]
        pub enum #ty {
            #(#enum_variants),*
        }

        impl<'a> TryFrom<crate::lexing::Token<'a>> for #ty {
            type Error = crate::lexing::Token<'a>;

            fn try_from(token: crate::lexing::Token<'a>) -> Result<Self, Self::Error> {
                Ok(match token {
                    #(#try_from_token_match_branches,)*
                    token => return Err(token),
                })
            }
        }

        impl<'a> Into<crate::lexing::Token<'a>> for #ty {
            fn into(self) -> crate::lexing::Token<'a> {
                match self {
                    #(#into_token_match_branches,)*
                }
            }
        }

        impl<'a> crate::lexing::TokenKind<'a> for #ty {
            const NAME: &'static str = stringify!(#name);
        }
    })
}

fn parse_input(input: ParseStream<'_>) -> parse::Result<(Ident, Vec<Variant>)> {
    let name = input.parse()?;

    let content;
    braced!(content in input);

    Ok((
        name,
        content
            .parse_terminated::<_, Token![,]>(Variant::parse)?
            .into_iter()
            .collect(),
    ))
}

struct Variant {
    name: Ident,
    has_lifetime: bool,
}

impl Variant {
    fn gen_enum_variant(&self) -> TokenStream {
        let name = &self.name;
        let lifetime = self.has_lifetime.then_some(quote!(<'a>));

        quote! {
            #name(crate::lexing::token::#name #lifetime)
        }
    }

    fn gen_try_from_token_match_branch(&self) -> TokenStream {
        let name = &self.name;

        quote! {
            crate::lexing::Token::#name(value) => Self::#name(value)
        }
    }

    fn gen_into_token_match_branch(&self) -> TokenStream {
        let name = &self.name;

        quote! {
            Self::#name(value) => crate::lexing::Token::#name(value)
        }
    }
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            name: input.parse()?,
            has_lifetime: input.peek(Token![<]) && {
                input.parse::<Token![<]>().unwrap();
                input.parse::<Lifetime>()?;
                input.parse::<Token![>]>()?;
                true
            },
        })
    }
}
