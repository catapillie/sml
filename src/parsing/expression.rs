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
}
