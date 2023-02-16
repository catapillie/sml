use crate::lexing::token::Token;

use super::expression::Expression;

#[derive(Debug)]
pub enum Statement<'a> {
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
    }
}
