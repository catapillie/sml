use super::{Expression, Statement};

#[derive(Debug)]
pub enum ParserAST<'a> {
    Statement(Statement<'a>),
    Expression(Expression<'a>),
}

impl<'a> From<Expression<'a>> for ParserAST<'a> {
    fn from(val: Expression<'a>) -> Self {
        ParserAST::Expression(val)
    }
}
