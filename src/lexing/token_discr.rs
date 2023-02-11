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

    Equal,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,

    Eof,
}

impl TokenDiscr {
    // returns None if lookahead is not an operator.
    #[rustfmt::skip]
    pub fn binary_operator_precedence(&self) -> Option<u8> {
        Some(
            match self {
                Self::Ampersand => 30,
                Self::Pipe      => 30,
                Self::Asterisk  => 30,
                Self::Slash     => 30,
                Self::Plus      => 20,
                Self::Minus     => 20,
                Self::Equal     => 10,
                _ => return None,
            }
        )
    }

    // returns None if lookahead is not a unary operator.
    #[rustfmt::skip]
    pub fn unary_operator_precedence(&self) -> Option<u8> {
        Some(
            match self {
                TokenDiscr::Plus  => 50,
                TokenDiscr::Minus => 50,
                _ => return None,
            }
        )
    }

    // returns None if lookahead is not an operator.
    #[rustfmt::skip]
    pub fn operator_associativity(&self) -> Option<Associativity> {
        Some(
            match self {
                TokenDiscr::Ampersand => Associativity::Left,
                TokenDiscr::Pipe      => Associativity::Left,
                TokenDiscr::Asterisk  => Associativity::Left,
                TokenDiscr::Slash     => Associativity::Left,
                TokenDiscr::Plus      => Associativity::Left,
                TokenDiscr::Minus     => Associativity::Left,
                TokenDiscr::Equal     => Associativity::Right,
                _ => return None,
            }
        )
    }
}
