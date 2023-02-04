#[derive(Debug, PartialEq)]
pub enum TokenKind {
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