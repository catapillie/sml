use super::{token_kind::TokenKind, token_span::TokenSpan};

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

    pub fn span(&self) -> TokenSpan {
        self.span
    }
}
