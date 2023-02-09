use std::borrow::Cow;

use super::{token_discr::TokenDiscr};

#[derive(Debug, PartialEq)]
#[derive(enum_assoc::Assoc)]
#[func(pub const fn discr(&self) -> TokenDiscr)]
#[rustfmt::skip]
pub enum TokenKind<'a> {
    #[assoc(discr = TokenDiscr::KeywordFn)] KeywordFn,
    #[assoc(discr = TokenDiscr::KeywordIf)] KeywordIf,
    #[assoc(discr = TokenDiscr::KeywordUnless)] KeywordUnless,
    #[assoc(discr = TokenDiscr::KeywordWhile)] KeywordWhile,
    #[assoc(discr = TokenDiscr::KeywordUntil)] KeywordUntil,
    #[assoc(discr = TokenDiscr::KeywordElse)] KeywordElse,
    #[assoc(discr = TokenDiscr::KeywordForever)] KeywordForever,
    #[assoc(discr = TokenDiscr::KeywordRepeat)] KeywordRepeat,

    #[assoc(discr = TokenDiscr::Identifier)] Identifier(&'a str),

    #[assoc(discr = TokenDiscr::Int)] Int(u64),
    #[assoc(discr = TokenDiscr::Int)] MalformedInt,

    #[assoc(discr = TokenDiscr::String)] String(Cow<'a, str>),
    #[assoc(discr = TokenDiscr::String)] MalformedString,

    #[assoc(discr = TokenDiscr::LeftParen)] LeftParen,
    #[assoc(discr = TokenDiscr::RightParen)] RightParen,
    #[assoc(discr = TokenDiscr::LeftBracket)] LeftBracket,
    #[assoc(discr = TokenDiscr::RightBracket)] RightBracket,
    #[assoc(discr = TokenDiscr::LeftBrace)] LeftBrace,
    #[assoc(discr = TokenDiscr::RightBrace)] RightBrace,
    #[assoc(discr = TokenDiscr::LeftChevron)] LeftChevron,
    #[assoc(discr = TokenDiscr::RightChevron)] RightChevron,
    
    #[assoc(discr = TokenDiscr::Dot)] Dot,
    #[assoc(discr = TokenDiscr::Comma)] Comma,
    #[assoc(discr = TokenDiscr::Colon)] Colon,
    #[assoc(discr = TokenDiscr::Semicolon)] Semicolon,

    #[assoc(discr = TokenDiscr::Equal)] Equal,
    #[assoc(discr = TokenDiscr::Plus)] Plus,
    #[assoc(discr = TokenDiscr::Minus)] Minus,
    #[assoc(discr = TokenDiscr::Asterisk)] Asterisk,
    #[assoc(discr = TokenDiscr::Slash)] Slash,
    #[assoc(discr = TokenDiscr::Ampersand)] Ampersand,
    #[assoc(discr = TokenDiscr::Pipe)] Pipe,

    #[assoc(discr = TokenDiscr::Eof)] Eof,

    #[assoc(discr = fake_discr(self))] Fake(TokenDiscr),
}

const fn fake_discr(kind: &TokenKind) -> TokenDiscr {
    let TokenKind::Fake(discr) = kind else {
        panic!("called `fake_discr` on a non-fake token kind");
    };
    *discr
}
