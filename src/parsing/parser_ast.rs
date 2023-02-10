use super::expression::Expression;

#[derive(Debug)]
pub enum ParserAST<'a>{
    Expression(Expression<'a>)
}

impl<'a> From<Expression<'a>> for ParserAST<'a> {
    fn from(val: Expression<'a>) -> Self {
        ParserAST::Expression(val)
    }
} 