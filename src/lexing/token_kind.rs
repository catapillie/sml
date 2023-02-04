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
    MalformedInt(&'a str),

    String(&'a str),
    MalformedString(&'a str),

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