mod cursor;
mod lexer;
mod location;
pub mod token;
mod token_kind;
mod token_span;

pub use {
    lexer::Lexer, location::Location, token::Token, token::TokenDiscr, token_kind::TokenKind,
    token_span::TokenSpan,
};

use cursor::Cursor;
