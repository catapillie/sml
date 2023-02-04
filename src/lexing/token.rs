use super::{token_kind::TokenKind, token_span::TokenSpan};

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    span: TokenSpan,
}

impl Token {
    pub const EOF: Self = Self {
        kind: TokenKind::Eof,
        span: TokenSpan::MAX,
    };

    pub fn new(kind: TokenKind, span: TokenSpan) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}
