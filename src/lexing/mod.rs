mod cursor;
mod lexer;
mod location;
mod token;
mod token_discr;
mod token_kind;
mod token_span;

pub use {
    lexer::Lexer, token::Token, token_discr::TokenDiscr, token_kind::TokenKind,
    token_span::TokenSpan,
};

use {cursor::Cursor, location::Location};
