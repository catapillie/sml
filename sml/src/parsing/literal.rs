use sml_macros::gen_token_kind;

gen_token_kind! {
    Literal {
        Int,
        Float,
        String<'a>,
        Character,
    }
}
