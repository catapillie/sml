use super::expression::Expression;
use super::statement::Statement;

#[derive(Debug)]
pub enum ParserAST<'a>{
    Statement(Statement<'a>),
    Expression(Expression<'a>)
}

impl<'a> From<Expression<'a>> for ParserAST<'a> {
    fn from(val: Expression<'a>) -> Self {
        ParserAST::Expression(val)
    }
} 