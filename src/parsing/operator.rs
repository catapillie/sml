use crate::{
    lexing::{token::Token, token_discr::TokenDiscr, token_kind::TokenKind},
    parsing::associativity::Associativity,
};

use super::priority::Priority;

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Ampersand,
    Pipe,
    Asterisk,
    Slash,

    Plus,
    Minus,

    LeftShift,
    RightShift,

    EqualEqual,
    NotEqual,
    LeftChevron,
    RightChevron,
    LessOrEqual,
    GreaterOrEqual,

    Equal,
    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    AmpersandEqual,
    PipeEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum PreUnaryOperator {
    Not,
    PlusPlus,
    MinusMinus,
    Plus,
    Minus,
}

#[derive(Debug, Clone, Copy)]
pub enum PostUnaryOperator {
    PlusPlus,
    MinusMinus,
}

impl BinaryOperator {
    pub fn priority(&self) -> Priority {
        Priority::of_binary_operator(*self)
    }

    #[rustfmt::skip]
    pub fn precedence(&self) -> u8 {
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
            Self::LeftChevron    => 20,
            Self::RightChevron   => 20,
            Self::LessOrEqual    => 20,
            Self::GreaterOrEqual => 20,


            Self::Equal          => 10,
            Self::PlusEqual      => 10,
            Self::MinusEqual     => 10,
            Self::AsteriskEqual  => 10,
            Self::SlashEqual     => 10,
            Self::AmpersandEqual => 10,
            Self::PipeEqual      => 10,
        }
    }

    #[rustfmt::skip]
    pub fn associativity(&self) -> Associativity {
        use Associativity::*;
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
            Self::LeftChevron    => Left,
            Self::RightChevron   => Left,
            Self::LessOrEqual    => Left,
            Self::GreaterOrEqual => Left,

            Self::Equal          => Right,
            Self::PlusEqual      => Right,
            Self::MinusEqual     => Right,
            Self::AsteriskEqual  => Right,
            Self::SlashEqual     => Right,
            Self::AmpersandEqual => Right,
            Self::PipeEqual      => Right,
        }
    }
}

impl PreUnaryOperator {
    pub fn priority(&self) -> Priority {
        Priority::of_pre_unary_operator(*self)
    }

    #[rustfmt::skip]
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Not        => 100,
            Self::PlusPlus   => 100,
            Self::MinusMinus => 100,
            Self::Plus       => 70,
            Self::Minus      => 70,
        }
    }
}

impl TryFrom<&TokenDiscr> for BinaryOperator {
    type Error = ();
    fn try_from(value: &TokenDiscr) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenDiscr::Ampersand => Self::Ampersand,
            TokenDiscr::Pipe => Self::Pipe,
            TokenDiscr::Asterisk => Self::Asterisk,
            TokenDiscr::Slash => Self::Slash,

            TokenDiscr::Plus => Self::Plus,
            TokenDiscr::Minus => Self::Minus,

            TokenDiscr::LeftShift => Self::LeftShift,
            TokenDiscr::RightShift => Self::RightShift,

            TokenDiscr::EqualEqual => Self::EqualEqual,
            TokenDiscr::NotEqual => Self::NotEqual,
            TokenDiscr::LeftChevron => Self::LeftChevron,
            TokenDiscr::RightChevron => Self::RightChevron,
            TokenDiscr::LessOrEqual => Self::LessOrEqual,
            TokenDiscr::GreaterOrEqual => Self::GreaterOrEqual,

            TokenDiscr::Equal => Self::Equal,
            TokenDiscr::PlusEqual => Self::PlusEqual,
            TokenDiscr::MinusEqual => Self::MinusEqual,
            TokenDiscr::AsteriskEqual => Self::AsteriskEqual,
            TokenDiscr::SlashEqual => Self::SlashEqual,
            TokenDiscr::AmpersandEqual => Self::AmpersandEqual,
            TokenDiscr::PipeEqual => Self::PipeEqual,

            _ => return Err(()),
        })
    }
}
impl<'a> TryFrom<&TokenKind<'a>> for BinaryOperator {
    type Error = ();
    fn try_from(value: &TokenKind<'a>) -> Result<Self, Self::Error> {
        (&value.discr()).try_into()
    }
}
impl<'a> TryFrom<&Token<'a>> for BinaryOperator {
    type Error = ();
    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        (&value.kind().discr()).try_into()
    }
}

impl TryFrom<&TokenDiscr> for PreUnaryOperator {
    type Error = ();
    fn try_from(value: &TokenDiscr) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenDiscr::Not => Self::Not,
            TokenDiscr::PlusPlus => Self::PlusPlus,
            TokenDiscr::MinusMinus => Self::MinusMinus,
            TokenDiscr::Plus => Self::Plus,
            TokenDiscr::Minus => Self::Minus,

            _ => return Err(()),
        })
    }
}
impl<'a> TryFrom<&TokenKind<'a>> for PreUnaryOperator {
    type Error = ();
    fn try_from(value: &TokenKind<'a>) -> Result<Self, Self::Error> {
        (&value.discr()).try_into()
    }
}
impl<'a> TryFrom<&Token<'a>> for PreUnaryOperator {
    type Error = ();
    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        (&value.kind().discr()).try_into()
    }
}

impl TryFrom<&TokenDiscr> for PostUnaryOperator {
    type Error = ();
    fn try_from(value: &TokenDiscr) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenDiscr::PlusPlus => Self::PlusPlus,
            TokenDiscr::MinusMinus => Self::MinusMinus,

            _ => return Err(()),
        })
    }
}
impl<'a> TryFrom<&TokenKind<'a>> for PostUnaryOperator {
    type Error = ();
    fn try_from(value: &TokenKind<'a>) -> Result<Self, Self::Error> {
        (&value.discr()).try_into()
    }
}
impl<'a> TryFrom<&Token<'a>> for PostUnaryOperator {
    type Error = ();
    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        (&value.kind().discr()).try_into()
    }
}
