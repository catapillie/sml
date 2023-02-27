use proc_macro::TokenStream;
use syn::Error;

mod token;
mod token_kind;

#[proc_macro]
pub fn gen_tokens(input: TokenStream) -> TokenStream {
    token::gen_tokens(input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn gen_token_kind(input: TokenStream) -> TokenStream {
    token_kind::gen_token_kind(input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
