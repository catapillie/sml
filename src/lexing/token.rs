use super::{token_discr::TokenDiscr, token_kind::TokenKind, token_span::TokenSpan};

#[derive(Debug)]
pub struct Token<'a> {
    kind: TokenKind<'a>,
    span: TokenSpan,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: TokenSpan) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn discr(&self) -> TokenDiscr {
        self.kind.discr()
    }

    pub fn span(&self) -> TokenSpan {
        self.span
    }
}
