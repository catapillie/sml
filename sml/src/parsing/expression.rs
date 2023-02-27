use crate::lexing::token::{Identifier, LeftParen, RightParen};

use super::{BinaryOperator, Fakable, Literal, PostUnaryOperator, PreUnaryOperator};

#[derive(Debug)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    Literal(Literal<'a>),
    Parenthesized {
        left_paren: LeftParen,
        expression: Fakable<Box<Expression<'a>>>,
        right_paren: Fakable<RightParen>,
    },
    BinaryOperation {
        left_operand: Box<Expression<'a>>,
        operator: BinaryOperator,
        right_operand: Fakable<Box<Expression<'a>>>,
    },
    PreUnaryOperation {
        operator: PreUnaryOperator,
        operand: Fakable<Box<Expression<'a>>>,
    },
    PostUnaryOperation {
        operand: Box<Expression<'a>>,
        operator: PostUnaryOperator,
    },
}
