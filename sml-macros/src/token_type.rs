use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    self, braced,
    parse::{self, Parse, ParseStream, Parser},
    Ident, Token,
};

fn parse_input(input: ParseStream<'_>) -> parse::Result<(Ident, Vec<Ident>)> {
    let name = input.parse()?;

    let content;
    braced!(content in input);

    Ok((
        name,
        content
            .parse_terminated::<_, Token![,]>(Ident::parse)?
            .into_iter()
            .collect(),
    ))
}

pub fn gen_token_type(input: TokenStream) -> syn::Result<TokenStream> {
    let (name, variants) = parse_input.parse2(input)?;

    Ok(quote! {
        #[derive(Debug)]
        pub enum #name {
            #(#variants(crate::lexing::token::#variants)),*
        }

        impl<'a> TryFrom<Token<'a>> for #name {
            type Error = Token<'a>;

            #[allow(unreachable)]
            fn try_from(token: Token<'a>) -> Result<Self, Token<'a>> {
                Ok(match token {
                    #(Token::#variants(value) => Self::#variants(value),)*
                    token => return Err(token),
                })
            }
        }

        impl Into<Token<'static>> for #name {
            fn into(self) -> Token<'static> {
                match self {
                    #(Self::#variants(value) => Token::#variants(value),)*
                }
            }
        }
    })
}
