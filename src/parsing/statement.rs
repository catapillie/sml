use crate::lexing::Token;

use super::Expression;

#[derive(Debug)]
pub enum Statement<'a> {
    None,
    Block {
        left_brace: Token<'a>,
        statements: Vec<Statement<'a>>,
        right_brace: Token<'a>,
    },
    If {
        if_token: Token<'a>,
        condition: Expression<'a>,
        statement: Box<Statement<'a>>,
    },
    Unless {
        unless_token: Token<'a>,
        condition: Expression<'a>,
        statement: Box<Statement<'a>>,
    },
    While {
        while_token: Token<'a>,
        condition: Expression<'a>,
        statement: Box<Statement<'a>>,
    },
    Until {
        until_token: Token<'a>,
        condition: Expression<'a>,
        statement: Box<Statement<'a>>,
    },
    Expression {
        expression: Expression<'a>,
        semicolon: Token<'a>,
    },
}
