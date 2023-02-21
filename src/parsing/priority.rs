use std::cmp::Ordering;

use super::{Associativity, BinaryOperator, PreUnaryOperator};

pub struct OperatorPriority {
    precedence: u8,
    associativity: Associativity,
}

impl OperatorPriority {
    pub fn of_binary_operator(operator: BinaryOperator) -> Self {
        Self {
            precedence: operator.precedence(),
            associativity: operator.associativity(),
        }
    }

    pub fn of_pre_unary_operator(operator: PreUnaryOperator) -> Self {
        Self {
            precedence: operator.precedence(),
            associativity: Associativity::Left,
        }
    }

    // we do this instead of just a method to compare two `Priority`s to avoid confusion
    // as this comparison is not commutative.
    pub fn is_greater_than_ahead(&self, ahead: &Self) -> bool {
        match self.precedence.cmp(&ahead.precedence) {
            Ordering::Greater => true,
            Ordering::Equal => matches!(self.associativity, Associativity::Left),
            Ordering::Less => false,
        }
    }
}

impl Default for OperatorPriority {
    fn default() -> Self {
        Self {
            precedence: 0,
            associativity: Associativity::Left,
        }
    }
}
