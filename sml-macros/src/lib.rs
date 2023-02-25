use proc_macro::TokenStream;
use syn::Error;

mod token;
mod token_type;

#[proc_macro]
pub fn gen_tokens(input: TokenStream) -> TokenStream {
    token::gen_tokens(input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn gen_token_type(input: TokenStream) -> TokenStream {
    token_type::gen_token_type(input.into())
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
