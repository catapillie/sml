use std::borrow::Cow;

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    KeywordFn,
    KeywordIf,
    KeywordUnless,
    KeywordWhile,
    KeywordUntil,
    KeywordElse,
    KeywordForever,
    KeywordRepeat,

    Identifier(&'a str),

    Int(u64),
    MalformedInt,

    String(Cow<'a, str>),
    MalformedString,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftChevron,
    RightChevron,

    Dot,
    Comma,
    Colon,
    Semicolon,

    Equal,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,

    Eof,
}
