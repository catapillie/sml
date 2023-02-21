use crate::{
    lexing::{Token, TokenDiscr, TokenKind},
    parsing::Associativity,
};

use super::OperatorPriority;

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
    BangEqual,
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
    Bang,
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
    #[rustfmt::skip]
    pub fn priority(&self) -> OperatorPriority {
        use BinaryOperator::*;
        
        let (precedence, associativity) = match self {
            Ampersand      => (50, Associativity::Left),
            Pipe           => (50, Associativity::Left),
            Asterisk       => (50, Associativity::Left),
            Slash          => (50, Associativity::Left),

            Plus           => (40, Associativity::Left),
            Minus          => (40, Associativity::Left),

            LeftShift      => (30, Associativity::Left),
            RightShift     => (30, Associativity::Left),

            EqualEqual     => (20, Associativity::Left),
            BangEqual      => (20, Associativity::Left),
            LeftChevron    => (20, Associativity::Left),
            RightChevron   => (20, Associativity::Left),
            LessOrEqual    => (20, Associativity::Left),
            GreaterOrEqual => (20, Associativity::Left),


            Equal          => (10, Associativity::Right),
            PlusEqual      => (10, Associativity::Right),
            MinusEqual     => (10, Associativity::Right),
            AsteriskEqual  => (10, Associativity::Right),
            SlashEqual     => (10, Associativity::Right),
            AmpersandEqual => (10, Associativity::Right),
            PipeEqual      => (10, Associativity::Right),
        };

        OperatorPriority::new(precedence, associativity)
    }
}

impl PreUnaryOperator {
    #[rustfmt::skip]
    pub fn priority(&self) -> OperatorPriority {
        OperatorPriority::new(
            match self {
                Self::Bang       => 100,
                Self::PlusPlus   => 100,
                Self::MinusMinus => 100,
                Self::Plus       => 70,
                Self::Minus      => 70,
            },
            Associativity::Left
        )
    }
}

impl TryFrom<&TokenDiscr> for BinaryOperator {
    type Error = ();

    #[rustfmt::skip]
    fn try_from(value: &TokenDiscr) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenDiscr::Ampersand      => Self::Ampersand,
            TokenDiscr::Pipe           => Self::Pipe,
            TokenDiscr::Asterisk       => Self::Asterisk,
            TokenDiscr::Slash          => Self::Slash,

            TokenDiscr::Plus           => Self::Plus,
            TokenDiscr::Minus          => Self::Minus,

            TokenDiscr::LeftShift      => Self::LeftShift,
            TokenDiscr::RightShift     => Self::RightShift,

            TokenDiscr::EqualEqual     => Self::EqualEqual,
            TokenDiscr::BangEqual      => Self::BangEqual,
            TokenDiscr::LeftChevron    => Self::LeftChevron,
            TokenDiscr::RightChevron   => Self::RightChevron,
            TokenDiscr::LessOrEqual    => Self::LessOrEqual,
            TokenDiscr::GreaterOrEqual => Self::GreaterOrEqual,

            TokenDiscr::Equal          => Self::Equal,
            TokenDiscr::PlusEqual      => Self::PlusEqual,
            TokenDiscr::MinusEqual     => Self::MinusEqual,
            TokenDiscr::AsteriskEqual  => Self::AsteriskEqual,
            TokenDiscr::SlashEqual     => Self::SlashEqual,
            TokenDiscr::AmpersandEqual => Self::AmpersandEqual,
            TokenDiscr::PipeEqual      => Self::PipeEqual,

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

    #[rustfmt::skip]
    fn try_from(value: &TokenDiscr) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenDiscr::Bang       => Self::Bang,
            TokenDiscr::PlusPlus   => Self::PlusPlus,
            TokenDiscr::MinusMinus => Self::MinusMinus,
            TokenDiscr::Plus       => Self::Plus,
            TokenDiscr::Minus      => Self::Minus,

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

    #[rustfmt::skip]
    fn try_from(value: &TokenDiscr) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenDiscr::PlusPlus   => Self::PlusPlus,
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
