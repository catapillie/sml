use super::expression::Expression;

#[derive(Debug)]
pub enum ParserAST<'a>{
    Expression(Box<Expression<'a>>)
}

impl<'a> From<Expression<'a>> for ParserAST<'a> {
    fn from(val: Expression<'a>) -> Self {
        ParserAST::Expression(Box::new(val))
    }
} 