#[derive(Debug, PartialEq, Copy, Clone)]
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
    Float,
    String,
    Character,

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

    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,

    EqualEqual,
    BangEqual,
    LessOrEqual,
    GreaterOrEqual,

    Equal,
    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    AmpersandEqual,
    PipeEqual,

    LeftShift,
    RightShift,

    Bang,
    PlusPlus,
    MinusMinus,

    Eof,
}
