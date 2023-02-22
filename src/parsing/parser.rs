use std::mem;

use crate::{
    diagnostics::{DiagnosticList, LexerDiagnosticKind, ParserDiagnosticKind},
    lexing::{Lexer, Token, TokenDiscr, TokenKind, TokenSpan},
};

use super::{BinaryOperator, Expression, OperatorPriority, PreUnaryOperator, Statement};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    diagnostics: DiagnosticList<ParserDiagnosticKind>,
    lookahead: Token<'a>,
}

impl<'a> Parser<'a> {
    // parser creation implies lexing the first token.
    // should this behavior be made explicit, i.e., in another function?
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let first_token = lexer.lex();
        Self {
            lexer,
            diagnostics: DiagnosticList::new(),
            lookahead: first_token,
        }
    }

    pub fn lexer_diagnostics(&self) -> &DiagnosticList<LexerDiagnosticKind> {
        self.lexer.diagnostics()
    }

    pub fn diagnostics(&self) -> &DiagnosticList<ParserDiagnosticKind> {
        &self.diagnostics
    }

    fn consume(&mut self) -> Token<'a> {
        // replace the lookahead with the new token, but return its previous value.
        mem::replace(&mut self.lookahead, self.lexer.lex())
    }

    fn expect(&mut self, expected: TokenDiscr) -> Token<'a> {
        let found = self.lookahead.discr();
        if found == expected {
            self.consume()
        } else {
            let span = self.lookahead.span();
            match found {
                TokenDiscr::Eof => self
                    .diagnostics
                    .push_kind(ParserDiagnosticKind::UnexpectedEof { expected }, span),
                _ => {
                    self.diagnostics.push_kind(
                        ParserDiagnosticKind::UnexpectedToken {
                            expected,
                            found: self.lookahead.discr(),
                        },
                        span,
                    );
                }
            }
            // we return a 'fake' token, because we aren't consuming the lookahead yet.
            let fake_span = TokenSpan::empty(self.lookahead.span().start());
            Token::new(TokenKind::Fake(expected), fake_span)
        }
    }

    fn try_expect(&mut self, expected: TokenDiscr) -> Option<Token<'a>> {
        if self.lookahead.discr() == expected {
            Some(self.consume())
        } else {
            None
        }
    }

    pub fn parse_statement(&mut self) -> Statement<'a> {
        while let TokenDiscr::Semicolon = self.lookahead.discr() {
            self.consume();
        }

        let statement = match self.lookahead.discr() {
            TokenDiscr::LeftBrace => {
                let left_brace = self.consume();

                let mut statements = Vec::new();

                #[rustfmt::skip]
                while !matches!(self.lookahead.discr(), TokenDiscr::RightBrace | TokenDiscr::Eof)
                {
                    statements.push(self.parse_statement());
                };

                let right_brace = self.expect(TokenDiscr::RightBrace);

                Statement::Block {
                    left_brace,
                    statements,
                    right_brace,
                }
            }

            TokenDiscr::KeywordIf => {
                let if_token = self.consume();
                let condition = self.parse_expression();
                let statement = Box::new(self.parse_statement());

                Statement::If {
                    if_token,
                    condition,
                    statement,
                }
            }

            TokenDiscr::KeywordUnless => {
                let unless_token = self.consume();
                let condition = self.parse_expression();
                let statement = Box::new(self.parse_statement());

                Statement::Unless {
                    unless_token,
                    condition,
                    statement,
                }
            }

            TokenDiscr::KeywordWhile => {
                let while_token = self.consume();
                let condition = self.parse_expression();
                let statement = Box::new(self.parse_statement());

                Statement::While {
                    while_token,
                    condition,
                    statement,
                }
            }

            TokenDiscr::KeywordUntil => {
                let until_token = self.consume();
                let condition = self.parse_expression();
                let statement = Box::new(self.parse_statement());

                Statement::Until {
                    until_token,
                    condition,
                    statement,
                }
            }

            _ => {
                if let Some(expression) = self.try_parse_expression() {
                    let semicolon = self.expect(TokenDiscr::Semicolon);
                    Statement::Expression {
                        expression,
                        semicolon,
                    }
                } else {
                    let tok = self.consume();
                    self.diagnostics
                        .push_kind(ParserDiagnosticKind::ExpectedStatement, tok.span());

                    Statement::None
                }
            }
        };

        while let TokenDiscr::Semicolon = self.lookahead.discr() {
            self.consume();
        }

        statement
    }

    fn try_parse_expression(&mut self) -> Option<Expression<'a>> {
        if self.is_expression_start() {
            Some(self.parse_expression())
        } else {
            None
        }
    }

    pub fn parse_expression(&mut self) -> Expression<'a> {
        self.parse_operation_expression(OperatorPriority::default())
    }

    fn parse_operation_expression(&mut self, priority: OperatorPriority) -> Expression<'a> {
        let mut left = if let Ok(pre_unary_operator) = PreUnaryOperator::try_from(&self.lookahead) {
            let priority_ahead = pre_unary_operator.priority();
            let operator = self.consume();
            Expression::UnaryOperation {
                operator,
                operand: Box::new(self.parse_operation_expression(priority_ahead)),
            }
        } else {
            self.parse_primary_expression()
        };

        loop {
            // no operator ahead, break and return immediately the primary expression.
            let Ok(binary_operator) = BinaryOperator::try_from(&self.lookahead) else {
                break;
            };

            let priority_ahead = binary_operator.priority();

            // the operator located ahead has lower priority.
            // we thus have to break early, return the current operator,
            // and let the previous call build the binary expression,
            // so that the tree accurately respects the order of operations that we defined.
            if priority.is_greater_than_ahead(&priority_ahead) {
                break;
            }

            // we can parse the operation here.
            // consume the operator, parse next operation *with higher precedence*.
            // put the left expression as left operand.
            let operator = self.consume();

            let right = self.parse_operation_expression(priority_ahead);
            left = Expression::BinaryOperation {
                left_operand: Box::new(left),
                operator,
                right_operand: Box::new(right),
            };
        }

        // finally return the built expression.
        left
    }

    // NOTE: beginning tokens of expressions MUST be added in `is_expression_start`!
    fn parse_primary_expression(&mut self) -> Expression<'a> {
        match self.lookahead.discr() {
            TokenDiscr::Identifier => Expression::Identifier {
                token: self.consume(),
            },
            TokenDiscr::Int | TokenDiscr::Float | TokenDiscr::String | TokenDiscr::Character => {
                Expression::Literal {
                    token: self.consume(),
                }
            }
            TokenDiscr::LeftParen => {
                let left_paren = self.consume();
                let expression = Box::new(self.parse_expression());
                let right_paren = self.expect(TokenDiscr::RightParen);
                Expression::Parenthesized {
                    left_paren,
                    expression,
                    right_paren,
                }
            }
            _ => {
                self.diagnostics.push_kind(
                    ParserDiagnosticKind::ExpectedExpression,
                    self.lookahead.span(),
                );
                Expression::None
            }
        }
    }

    #[rustfmt::skip]
    fn is_expression_start(&self) -> bool {
        let discr = self.lookahead.discr();

        // pre-unary operators are beginning of expressions.
        if PreUnaryOperator::try_from(&self.lookahead).is_ok() {
            return true;
        }

        // the tokens here are also beginning of expressions.
        matches!(discr, TokenDiscr::Int
                      | TokenDiscr::Float
                      | TokenDiscr::String
                      | TokenDiscr::Character
                      | TokenDiscr::Identifier
                      | TokenDiscr::LeftParen)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{lexing::TokenKind, parsing::parser_ast::ParserAST};

    #[test]
    fn test_if_statement() {
        assert_eq!(
            TestStatement::If {
                condition: TestExpression::BinaryOperation {
                    left_operand: Box::new(TestExpression::Identifier {
                        token: TokenKind::Identifier("a"),
                    }),
                    operator: TokenKind::EqualEqual,
                    right_operand: Box::new(TestExpression::Identifier {
                        token: TokenKind::Identifier("b"),
                    }),
                },
                statement: Box::new(TestStatement::Block {
                    statements: vec![TestStatement::Expression {
                        expression: TestExpression::BinaryOperation {
                            left_operand: Box::new(TestExpression::Identifier {
                                token: TokenKind::Identifier("c"),
                            }),
                            operator: TokenKind::Equal,
                            right_operand: Box::new(TestExpression::Identifier {
                                token: TokenKind::Identifier("d"),
                            }),
                        },
                    }],
                })
            },
            Parser::new("if a == b { c = d; }").parse_statement(),
        );
    }

    #[test]
    fn test_assignment_expression() {
        assert_eq!(
            TestExpression::BinaryOperation {
                left_operand: Box::new(TestExpression::Identifier {
                    token: TokenKind::Identifier("a"),
                }),
                operator: TokenKind::Equal,

                right_operand: Box::new(TestExpression::BinaryOperation {
                    left_operand: Box::new(TestExpression::Identifier {
                        token: TokenKind::Identifier("b"),
                    }),
                    operator: TokenKind::Equal,

                    right_operand: Box::new(TestExpression::BinaryOperation {
                        left_operand: Box::new(TestExpression::Identifier {
                            token: TokenKind::Identifier("c"),
                        }),
                        operator: TokenKind::Equal,

                        right_operand: Box::new(TestExpression::Identifier {
                            token: TokenKind::Identifier("d"),
                        })
                    })
                })
            },
            Parser::new("a = b = c = d").parse_expression(),
        );
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    enum TestParserAST {
        Statement(TestStatement),
        Expression(TestExpression),
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    enum TestStatement {
        None,
        Block {
            statements: Vec<TestStatement>,
        },
        If {
            condition: TestExpression,
            statement: Box<TestStatement>,
        },
        Unless {
            condition: TestExpression,
            statement: Box<TestStatement>,
        },
        While {
            condition: TestExpression,
            statement: Box<TestStatement>,
        },
        Until {
            condition: TestExpression,
            statement: Box<TestStatement>,
        },
        Expression {
            expression: TestExpression,
        },
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    enum TestExpression {
        None,
        Identifier {
            token: TokenKind<'static>,
        },
        Literal {
            token: TokenKind<'static>,
        },
        Parenthesized {
            expression: Box<TestExpression>,
        },
        BinaryOperation {
            left_operand: Box<TestExpression>,
            operator: TokenKind<'static>,
            right_operand: Box<TestExpression>,
        },
        UnaryOperation {
            operator: TokenKind<'static>,
            operand: Box<TestExpression>,
        },
    }

    impl<'a> PartialEq<ParserAST<'a>> for TestParserAST {
        fn eq(&self, other: &ParserAST<'a>) -> bool {
            match self {
                Self::Statement(statement) => {
                    matches!(other, ParserAST::Statement(other_statement) if statement == other_statement)
                }
                Self::Expression(expression) => {
                    matches!(other, ParserAST::Expression(other_expression) if expression == other_expression)
                }
            }
        }
    }

    impl<'a> PartialEq<Statement<'a>> for TestStatement {
        fn eq(&self, other: &Statement<'a>) -> bool {
            match self {
                Self::None => matches!(other, Statement::None),
                Self::Block { statements } => {
                    matches!(other,
                        Statement::Block {
                            statements: other_statements,
                            ..
                        }
                        if statements == other_statements
                    )
                }
                Self::If {
                    condition,
                    statement,
                } => {
                    matches!(other,
                        Statement::If {
                            condition: other_condition,
                            statement: other_statement,
                            ..
                        }
                        if condition == other_condition && **statement == **other_statement
                    )
                }
                Self::Unless {
                    condition,
                    statement,
                } => {
                    matches!(other,
                        Statement::Unless {
                            condition: other_condition,
                            statement: other_statement,
                            ..
                        }
                        if condition == other_condition && **statement == **other_statement
                    )
                }
                Self::While {
                    condition,
                    statement,
                } => {
                    matches!(other,
                        Statement::While {
                            condition: other_condition,
                            statement: other_statement,
                            ..
                        }
                        if condition == other_condition && **statement == **other_statement
                    )
                }
                Self::Until {
                    condition,
                    statement,
                } => {
                    matches!(other,
                        Statement::Until {
                            condition: other_condition,
                            statement: other_statement,
                            ..
                        }
                        if condition == other_condition && **statement == **other_statement
                    )
                }
                Self::Expression { expression, .. } => {
                    matches!(other,
                        Statement::Expression {
                            expression: other_expression,
                            ..
                        }
                        if expression == other_expression
                    )
                }
            }
        }
    }

    impl<'a> PartialEq<Expression<'a>> for TestExpression {
        fn eq(&self, other: &Expression<'a>) -> bool {
            match self {
                Self::None => matches!(other, Expression::None),
                Self::Identifier { token } => {
                    matches!(other,
                        Expression::Identifier {
                            token: other_token
                        }
                        if token == other_token.kind()
                    )
                }
                Self::Literal { token } => {
                    matches!(other,
                        Expression::Literal {
                            token: other_token
                        }
                        if token == other_token.kind()
                    )
                }
                Self::Parenthesized { expression } => {
                    matches!(other,
                        Expression::Parenthesized {
                            expression: other_expression, ..} if **expression == **other_expression)
                }
                Self::BinaryOperation {
                    left_operand,
                    operator,
                    right_operand,
                } => {
                    matches!(other,
                        Expression::BinaryOperation {
                            left_operand: other_left_operand,
                            operator: other_operator, right_operand: other_right_operand
                        }
                        if **left_operand == **other_left_operand && operator == other_operator.kind() && **right_operand == **other_right_operand
                    )
                }
                Self::UnaryOperation { operator, operand } => {
                    matches!(other,
                        Expression::UnaryOperation {
                            operator: other_operator,
                            operand: other_operand
                        }
                        if operator == other_operator.kind() && **operand == **other_operand
                    )
                }
            }
        }
    }
}
