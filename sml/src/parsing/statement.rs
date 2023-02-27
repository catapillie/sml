use crate::lexing::token::{
    KeywordIf, KeywordUnless, KeywordUntil, KeywordWhile, LeftBrace, RightBrace, Semicolon,
};

use super::{Expression, Fakable};

#[derive(Debug)]
pub enum Statement<'a> {
    Block {
        left_brace: LeftBrace,
        statements: Vec<Statement<'a>>,
        right_brace: Fakable<RightBrace>,
    },
    If {
        if_token: KeywordIf,
        condition: Fakable<Expression<'a>>,
        statement: Fakable<Box<Statement<'a>>>,
    },
    Unless {
        unless_token: KeywordUnless,
        condition: Fakable<Expression<'a>>,
        statement: Fakable<Box<Statement<'a>>>,
    },
    While {
        while_token: KeywordWhile,
        condition: Fakable<Expression<'a>>,
        statement: Fakable<Box<Statement<'a>>>,
    },
    Until {
        until_token: KeywordUntil,
        condition: Fakable<Expression<'a>>,
        statement: Fakable<Box<Statement<'a>>>,
    },
    Expression {
        expression: Expression<'a>,
        semicolon: Fakable<Semicolon>,
    },
    Empty {
        semicolon: Semicolon,
    },
}
