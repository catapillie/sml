use crate::parsing::associativity::Associativity;

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

    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,

    EqualEqual,
    NotEqual,
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

    Not,
    PlusPlus,
    MinusMinus,

    Eof,
}

impl TokenDiscr {
    // returns None if lookahead is not an operator.
    #[rustfmt::skip]
    pub fn binary_operator_precedence(&self) -> Option<u8> {
        Some(
            match self {
                Self::Ampersand      => 50,
                Self::Pipe           => 50,
                Self::Asterisk       => 50,
                Self::Slash          => 50,

                Self::Plus           => 40,
                Self::Minus          => 40,

                Self::LeftShift      => 30,
                Self::RightShift     => 30,

                Self::EqualEqual     => 20,
                Self::NotEqual       => 20,
                Self::LessOrEqual    => 20,
                Self::GreaterOrEqual => 20,


                Self::Equal          => 10,
                Self::PlusEqual      => 10,
                Self::MinusEqual     => 10,
                Self::AsteriskEqual  => 10,
                Self::SlashEqual     => 10,
                Self::AmpersandEqual => 10,
                Self::PipeEqual      => 10,
                _ => return None,
            }
        )
    }

    // returns None if lookahead is not a unary operator.
    #[rustfmt::skip]
    pub fn unary_operator_precedence(&self) -> Option<u8> {
        Some(
            match self {
                Self::Not        => 100,
                Self::PlusPlus   => 100,
                Self::MinusMinus => 100,
                Self::Plus       => 70,
                Self::Minus      => 70,
                _ => return None,
            }
        )
    }

    // returns None if lookahead is not an operator.
    #[rustfmt::skip]
    pub fn operator_associativity(&self) -> Option<Associativity> {
        use Associativity::*;
        Some(
            match self {
                Self::Ampersand      => Left,
                Self::Pipe           => Left,
                Self::Asterisk       => Left,
                Self::Slash          => Left,

                Self::Plus           => Left,
                Self::Minus          => Left,

                Self::LeftShift      => Left,
                Self::RightShift     => Left,


                Self::EqualEqual     => Left,
                Self::NotEqual       => Left,
                Self::LessOrEqual    => Left,
                Self::GreaterOrEqual => Left,

                Self::Equal          => Right,
                Self::PlusEqual      => Right,
                Self::MinusEqual     => Right,
                Self::AsteriskEqual  => Right,
                Self::SlashEqual     => Right,
                Self::AmpersandEqual => Right,
                Self::PipeEqual      => Right,
                _ => return None,
            }
        )
    }
}
