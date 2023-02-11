use crate::lexing::token::Token;

#[derive(Debug)]
pub enum Expression<'a> {
    None,
    Literal {
        token: Token<'a>,
    },
    Parenthesized {
        left_paren: Token<'a>,
        expression: Box<Expression<'a>>,
        right_paren: Token<'a>,
    },
    BinaryOperation {
        left_operand: Box<Expression<'a>>,
        operator: Token<'a>,
        right_operand: Box<Expression<'a>>,
    },
    UnaryOperation {
        operator: Token<'a>,
        operand: Box<Expression<'a>>,
    },
}
