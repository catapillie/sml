#[derive(Debug, PartialEq)]
pub enum TokenDiscr {
    KeywordFn,
    KeywordIf,
    KeywordUnless,
    KeywordWhile,
    KeywordUntil,
    KeywordElse,
    KeywordForever,
    KeywordRepeat,

    Identifier,
    Int,
    String,

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