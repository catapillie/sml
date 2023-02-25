use crate::{
    lexing::Token,
    parsing::Associativity,
};

use super::OperatorPriority;

use sml_macros::gen_token_type;

gen_token_type! {
    BinaryOperator {
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
}

gen_token_type! {
    PreUnaryOperator {
        Bang,
        PlusPlus,
        MinusMinus,
        Plus,
        Minus,
    }
}

gen_token_type! {
    PostUnaryOperator {
        PlusPlus,
        MinusMinus,
    }
}

impl BinaryOperator {
    #[rustfmt::skip]
    pub fn priority(&self) -> OperatorPriority {
        use BinaryOperator::*;
        
        let (precedence, associativity) = match self {
            Ampersand(_)      => (50, Associativity::Left),
            Pipe(_)           => (50, Associativity::Left),
            Asterisk(_)       => (50, Associativity::Left),
            Slash(_)          => (50, Associativity::Left),

            Plus(_)           => (40, Associativity::Left),
            Minus(_)          => (40, Associativity::Left),

            LeftShift(_)      => (30, Associativity::Left),
            RightShift(_)     => (30, Associativity::Left),

            EqualEqual(_)     => (20, Associativity::Left),
            BangEqual(_)      => (20, Associativity::Left),
            LeftChevron(_)    => (20, Associativity::Left),
            RightChevron(_)   => (20, Associativity::Left),
            LessOrEqual(_)    => (20, Associativity::Left),
            GreaterOrEqual(_) => (20, Associativity::Left),


            Equal(_)          => (10, Associativity::Right),
            PlusEqual(_)      => (10, Associativity::Right),
            MinusEqual(_)     => (10, Associativity::Right),
            AsteriskEqual(_)  => (10, Associativity::Right),
            SlashEqual(_)     => (10, Associativity::Right),
            AmpersandEqual(_) => (10, Associativity::Right),
            PipeEqual(_)      => (10, Associativity::Right),
        };

        OperatorPriority::new(precedence, associativity)
    }
}

impl PreUnaryOperator {
    #[rustfmt::skip]
    pub fn priority(&self) -> OperatorPriority {
        OperatorPriority::new(
            match self {
                Self::Bang(_)       => 100,
                Self::PlusPlus(_)   => 100,
                Self::MinusMinus(_) => 100,
                Self::Plus(_)       => 70,
                Self::Minus(_)      => 70,
            },
            Associativity::Left
        )
    }
}
