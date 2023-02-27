use std::mem;

use crate::{
    diagnostics::{DiagnosticsList, ParserDiagnostic},
    lexing::{token::*, Lexer, Location, Token, TokenDiscr, TokenKind, TokenSpan},
};

use super::{
    BinaryOperator, Expression, Fakable, Literal, OperatorPriority, PreUnaryOperator, Statement,
};

pub struct Parser<'a, 'b> {
    lexer: Lexer<'a, 'b>,
    diagnostics: &'b DiagnosticsList<'a>,
    lookahead: Token<'a>,
}

impl<'a: 'b, 'b> Parser<'a, 'b> {
    // parser creation implies lexing the first token.
    // should this behavior be made explicit, i.e., in another function?
    pub fn new(source: &'a str, diagnostics: &'b DiagnosticsList<'a>) -> Self {
        let mut lexer = Lexer::new(source, diagnostics);
        let first_token = lexer.lex();
        Self {
            lexer,
            diagnostics,
            lookahead: first_token,
        }
    }

    fn consume(&mut self) -> Token<'a> {
        // replace the lookahead with the new token, but return its previous value.
        mem::replace(&mut self.lookahead, self.lexer.lex())
    }

    fn expect<T: TokenKind<'a>>(&mut self) -> Fakable<T> {
        match self.try_expect() {
            Some(tok) => Fakable::Real(tok),
            None => {
                let span = self.lookahead.span();
                match self.lookahead {
                    Token::Eof(_) => self
                        .diagnostics
                        .push((ParserDiagnostic::UnexpectedEof { expected: T::NAME }, span).into()),
                    _ => {
                        self.diagnostics.push(
                            (
                                ParserDiagnostic::UnexpectedToken {
                                    expected: T::NAME,
                                    found: self.lookahead.clone(),
                                },
                                span,
                            )
                                .into(),
                        );
                    }
                }

                Fakable::Fake
            }
        }
    }

    fn try_expect<T: TokenKind<'a>>(&mut self) -> Option<T> {
        match mem::replace(
            &mut self.lookahead,
            Token::eof(TokenSpan::empty(Location::new(0, 0, 0))),
        )
        .try_into()
        {
            Ok(tok) => {
                self.consume();
                Some(tok)
            }
            Err(tok) => {
                self.lookahead = tok;
                None
            }
        }
    }

    pub fn parse_statement(&mut self, consume_unexpected: bool) -> Fakable<Statement<'a>> {
        Fakable::Real(match_expect! {self;
            (left_brace): LeftBrace => {
                let mut statements = Vec::new();

                #[rustfmt::skip]
                while !matches!(self.lookahead.discr(), TokenDiscr::RightBrace | TokenDiscr::Eof)
                {
                    if let Fakable::Real(statement) = self.parse_statement(true) {
                        statements.push(statement);
                    }
                };

                let right_brace = self.expect();

                Statement::Block {
                    left_brace,
                    statements,
                    right_brace,
                }
            },

            (if_token): KeywordIf => {
                let condition = self.parse_expression(false);
                let statement = self.parse_statement(false).box_inner();

                Statement::If {
                    if_token,
                    condition,
                    statement,
                }
            },

            (unless_token): KeywordUnless => {
                let condition = self.parse_expression(false);
                let statement = self.parse_statement(false).box_inner();

                Statement::Unless {
                    unless_token,
                    condition,
                    statement,
                }
            },

            (while_token): KeywordWhile => {
                let condition = self.parse_expression(false);
                let statement = self.parse_statement(false).box_inner();

                Statement::While {
                    while_token,
                    condition,
                    statement,
                }
            },

            (until_token): KeywordUntil => {
                let condition = self.parse_expression(false);
                let statement = self.parse_statement(false).box_inner();

                Statement::Until {
                    until_token,
                    condition,
                    statement,
                }
            },

            (semicolon): Semicolon => {
                Statement::Empty {
                    semicolon,
                }
            },

            _ => {
                if let Some(expression) = self.try_parse_expression() {
                    let semicolon = self.expect();
                    Statement::Expression {
                        expression,
                        semicolon,
                    }
                } else {
                    let tok = if consume_unexpected { self.consume() } else {self.lookahead.clone()};
                    let span = tok.span();
                    self.diagnostics
                        .push((ParserDiagnostic::ExpectedStatement {found: tok}, span).into());

                    return Fakable::Fake;
                }
            },
        })
    }

    pub fn parse_expression(&mut self, consume_unexpected: bool) -> Fakable<Expression<'a>> {
        self.parse_operation_expression(OperatorPriority::default(), consume_unexpected)
    }

    pub fn try_parse_expression(&mut self) -> Option<Expression<'a>> {
        self.try_parse_operation_expression(OperatorPriority::default())
    }

    fn parse_operation_expression(
        &mut self,
        priority: OperatorPriority,
        consume_unexpected: bool,
    ) -> Fakable<Expression<'a>> {
        match self.try_parse_operation_expression(priority) {
            Some(expression) => Fakable::Real(expression),
            None => {
                let tok = if consume_unexpected {
                    self.consume()
                } else {
                    self.lookahead.clone()
                };
                let span = tok.span();
                self.diagnostics
                    .push((ParserDiagnostic::ExpectedExpression { found: tok }, span).into());

                Fakable::Fake
            }
        }
    }

    fn try_parse_operation_expression(
        &mut self,
        priority: OperatorPriority,
    ) -> Option<Expression<'a>> {
        let mut left = match self.try_expect::<PreUnaryOperator>() {
            Some(pre_unary_operator) => {
                let priority_ahead = pre_unary_operator.priority();
                Expression::PreUnaryOperation {
                    operator: pre_unary_operator,
                    operand: self
                        .parse_operation_expression(priority_ahead, false)
                        .box_inner(),
                }
            }
            None => self.parse_primary_expression()?,
        };

        loop {
            // no operator ahead, break and return immediately the primary expression.
            let Some(binary_operator) = self.try_expect::<BinaryOperator>() else {
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
            let right = self.parse_operation_expression(priority_ahead, false);
            left = Expression::BinaryOperation {
                left_operand: Box::new(left),
                operator: binary_operator,
                right_operand: right.box_inner(),
            };
        }

        // finally return the built expression.
        Some(left)
    }

    // NOTE: beginning tokens of expressions MUST be added in `is_expression_start`!
    fn parse_primary_expression(&mut self) -> Option<Expression<'a>> {
        Some(match_expect! {self;
            (identifier): Identifier => Expression::Identifier(identifier),
            (literal): Literal => Expression::Literal(literal),
            (left_paren): LeftParen => {
                let expression = self.parse_expression(false).box_inner();
                let right_paren = self.expect();
                Expression::Parenthesized {
                    left_paren,
                    expression,
                    right_paren,
                }
            },
            _ => return None,
        })
    }
}

macro_rules! match_expect {
    (
        $self:ident;
        $(
            ($pat:pat): $ty:ty => $expr:expr,
        )* _ => $else:expr$(,)?
    ) => {
        loop {
            $(
                if let Some($pat) = $self.try_expect::<$ty>() {
                    break $expr;
                }
            )*
            #[allow(unreachable_code)]
            break {
                $else
            };
        }
    };
}

use match_expect;

// #[cfg(test)]
// mod tests {
//     use super::*;

//     use crate::{lexing::TokenKind, parsing::parser_ast::ParserAST};

//     // macro_rules! test_parser {
//     //     (@match_should_diagnostic should_diagnostic) => {};
//     //     ($(
//     //         $name:ident $($should_diagnostic:ident)? {
//     //             $raw:literal => $($tree:tt)*
//     //         }
//     //     )*) => {
//     //         $(test_parser! {@match_should_diagnostic $should_diagnostic})?

//     //         #[test]
//     //         fn $name() {
//     //             use crate::{Parser, DiagnosticsList};

//     //             let diagnostics = DiagosticsList::new();
//     //             let statement = Parser::new($raw, &diagnostics).parse_statement(false);

//     //             let diagnostics = diagnostics.to_vec();

//     //             if $(true ||)? false {
//     //                 if diagnostics.is_empty() {
//     //                     panic!("Expected diagnostics but none were outputed");
//     //                 }
//     //             } else {
//     //                 if !diagnostics.is_empty() {
//     //                     panic!("Parsing failed with the following diagnostics: {:?}", diagnostics);
//     //                 }
//     //             }

//     //             assert_eq_statement!(statement, $($tree)*);
//     //         }
//     //     };
//     // }
//     // macro_rules! assert_eq_statement {
//     //     ($statement:expr, {$($tt:tt;)*} $($($other:tt)*)?) => {
//     //         {
//     //             let statements = match $statement {
//     //                 Statements::Block { statements, .. } => statements,
//     //                 other => panic!("Expected \"{}\", found \"{:?}\" instead.", other),
//     //             };

//     //             let mut i = 0;

//     //             assert_eq_statement(statements[i], $(tt)*);
//     //         }

//     //         ${
//     //             i += 1;
//     //             assert_eq_statement($statement, $($other)*);
//     //         }?
//     //     };
//     //     ($statement:expr, if {$($tt:tt)*} $($($other:tt)*)?) => {
//     //         {
//     //             let statements = match $statement {
//     //                 Statements::Block { statements, .. } => statements,
//     //                 other => panic!("Expected \"{}\", found \"{:?}\" instead.", other),
//     //             };

//     //             let mut i = 0;

//     //             assert_eq_statement(statements[i], $(tt)*);
//     //         }

//     //         ${
//     //             i += 1;
//     //             assert_eq_statement($statement, $($other)*);
//     //         }?
//     //     };
//     // Block {
//     //     left_brace: LeftBrace,
//     //     statements: Vec<Statement<'a>>,
//     //     right_brace: Fakable<RightBrace>,
//     // },
//     // If {
//     //     if_token: KeywordIf,
//     //     condition: Fakable<Expression<'a>>,
//     //     statement: Fakable<Box<Statement<'a>>>,
//     // },
//     // Unless {
//     //     unless_token: KeywordUnless,
//     //     condition: Fakable<Expression<'a>>,
//     //     statement: Fakable<Box<Statement<'a>>>,
//     // },
//     // While {
//     //     while_token: KeywordWhile,
//     //     condition: Fakable<Expression<'a>>,
//     //     statement: Fakable<Box<Statement<'a>>>,
//     // },
//     // Until {
//     //     until_token: KeywordUntil,
//     //     condition: Fakable<Expression<'a>>,
//     //     statement: Fakable<Box<Statement<'a>>>,
//     // },
//     // Expression {
//     //     expression: Expression<'a>,
//     //     semicolon: Fakable<Semicolon>,
//     // },
//     // Empty {
//     //     semicolon: Semicolon,
//     // },
//     // }

//     #[test]
//     fn test_if_statement() {
//         assert_eq!(
//             TestStatement::If {
//                 condition: TestExpression::BinaryOperation {
//                     left_operand: Box::new(TestExpression::Identifier {
//                         token: TokenKind::Identifier("a"),
//                     }),
//                     operator: TokenKind::EqualEqual,
//                     right_operand: Box::new(TestExpression::Identifier {
//                         token: TokenKind::Identifier("b"),
//                     }),
//                 },
//                 statement: Box::new(TestStatement::Block {
//                     statements: vec![TestStatement::Expression {
//                         expression: TestExpression::BinaryOperation {
//                             left_operand: Box::new(TestExpression::Identifier {
//                                 token: TokenKind::Identifier("c"),
//                             }),
//                             operator: TokenKind::Equal,
//                             right_operand: Box::new(TestExpression::Identifier {
//                                 token: TokenKind::Identifier("d"),
//                             }),
//                         },
//                     }],
//                 })
//             },
//             Parser::new("if a == b { c = d; }").parse_statement(),
//         );
//     }

//     #[test]
//     fn test_assignment_expression() {
//         assert_eq!(
//             TestExpression::BinaryOperation {
//                 left_operand: Box::new(TestExpression::Identifier {
//                     token: TokenKind::Identifier("a"),
//                 }),
//                 operator: TokenKind::Equal,

//                 right_operand: Box::new(TestExpression::BinaryOperation {
//                     left_operand: Box::new(TestExpression::Identifier {
//                         token: TokenKind::Identifier("b"),
//                     }),
//                     operator: TokenKind::Equal,

//                     right_operand: Box::new(TestExpression::BinaryOperation {
//                         left_operand: Box::new(TestExpression::Identifier {
//                             token: TokenKind::Identifier("c"),
//                         }),
//                         operator: TokenKind::Equal,

//                         right_operand: Box::new(TestExpression::Identifier {
//                             token: TokenKind::Identifier("d"),
//                         })
//                     })
//                 })
//             },
//             Parser::new("a = b = c = d").parse_expression(),
//         );
//     }

//     #[allow(dead_code)]
//     #[derive(Debug)]
//     enum TestParserAST {
//         Statement(TestStatement),
//         Expression(TestExpression),
//     }

//     #[allow(dead_code)]
//     #[derive(Debug)]
//     enum TestStatement {
//         Block {
//             left_brace: LeftBrace,
//             statements: Vec<TestStatement<'a>>,
//             right_brace: Fakable<RightBrace>,
//         },
//         If {
//             if_token: KeywordIf,
//             condition: Fakable<TestExpression<'a>>,
//             statement: Fakable<Box<TestStatement<'a>>>,
//         },
//         Unless {
//             unless_token: KeywordUnless,
//             condition: Fakable<TestExpression<'a>>,
//             statement: Fakable<Box<TestStatement<'a>>>,
//         },
//         While {
//             while_token: KeywordWhile,
//             condition: Fakable<TestExpression<'a>>,
//             statement: Fakable<Box<TestStatement<'a>>>,
//         },
//         Until {
//             until_token: KeywordUntil,
//             condition: Fakable<TestExpression<'a>>,
//             statement: Fakable<Box<TestStatement<'a>>>,
//         },
//         Expression {
//             expression: TestExpression<'a>,
//             semicolon: Fakable<Semicolon>,
//         },
//         Empty {
//             semicolon: Semicolon,
//         },
//     }

//     #[allow(dead_code)]
//     #[derive(Debug)]
//     enum TestExpression {
//         Identifier(Identifier<'a>),
//         Literal(Literal<'a>),
//         Parenthesized {
//             left_paren: LeftParen,
//             expression: Fakable<Box<TestExpression<'a>>>,
//             right_paren: Fakable<RightParen>,
//         },
//         BinaryOperation {
//             left_operand: Box<TestExpression<'a>>,
//             operator: BinaryOperator,
//             right_operand: Fakable<Box<TestExpression<'a>>>,
//         },
//         PreUnaryOperation {
//             operator: PreUnaryOperator,
//             operand: Fakable<Box<TestExpression<'a>>>,
//         },
//         PostUnaryOperation {
//             operand: Box<TestExpression<'a>>,
//             operator: PostUnaryOperator,
//         },
//     }

//     impl<'a> PartialEq<ParserAST<'a>> for TestParserAST {
//         fn eq(&self, other: &ParserAST<'a>) -> bool {
//             match self {
//                 Self::Statement(statement) => {
//                     matches!(other, ParserAST::Statement(other_statement) if statement == other_statement)
//                 }
//                 Self::Expression(expression) => {
//                     matches!(other, ParserAST::Expression(other_expression) if expression == other_expression)
//                 }
//             }
//         }
//     }

//     impl<'a> PartialEq<Statement<'a>> for TestStatement {
//         fn eq(&self, other: &Statement<'a>) -> bool {
//             match self {
//                 Self::None => matches!(other, Statement::None),
//                 Self::Block { statements } => {
//                     matches!(other,
//                         Statement::Block {
//                             statements: other_statements,
//                             ..
//                         }
//                         if statements == other_statements
//                     )
//                 }
//                 Self::If {
//                     condition,
//                     statement,
//                 } => {
//                     matches!(other,
//                         Statement::If {
//                             condition: other_condition,
//                             statement: other_statement,
//                             ..
//                         }
//                         if condition == other_condition && **statement == **other_statement
//                     )
//                 }
//                 Self::Unless {
//                     condition,
//                     statement,
//                 } => {
//                     matches!(other,
//                         Statement::Unless {
//                             condition: other_condition,
//                             statement: other_statement,
//                             ..
//                         }
//                         if condition == other_condition && **statement == **other_statement
//                     )
//                 }
//                 Self::While {
//                     condition,
//                     statement,
//                 } => {
//                     matches!(other,
//                         Statement::While {
//                             condition: other_condition,
//                             statement: other_statement,
//                             ..
//                         }
//                         if condition == other_condition && **statement == **other_statement
//                     )
//                 }
//                 Self::Until {
//                     condition,
//                     statement,
//                 } => {
//                     matches!(other,
//                         Statement::Until {
//                             condition: other_condition,
//                             statement: other_statement,
//                             ..
//                         }
//                         if condition == other_condition && **statement == **other_statement
//                     )
//                 }
//                 Self::Expression { expression, .. } => {
//                     matches!(other,
//                         Statement::Expression {
//                             expression: other_expression,
//                             ..
//                         }
//                         if expression == other_expression
//                     )
//                 }
//             }
//         }
//     }

//     impl<'a> PartialEq<Expression<'a>> for TestExpression {
//         fn eq(&self, other: &Expression<'a>) -> bool {
//             match self {
//                 Self::None => matches!(other, Expression::None),
//                 Self::Identifier { token } => {
//                     matches!(other,
//                         Expression::Identifier {
//                             token: other_token
//                         }
//                         if token == other_token.kind()
//                     )
//                 }
//                 Self::Literal { token } => {
//                     matches!(other,
//                         Expression::Literal {
//                             token: other_token
//                         }
//                         if token == other_token.kind()
//                     )
//                 }
//                 Self::Parenthesized { expression } => {
//                     matches!(other,
//                         Expression::Parenthesized {
//                             expression: other_expression, ..} if **expression == **other_expression)
//                 }
//                 Self::BinaryOperation {
//                     left_operand,
//                     operator,
//                     right_operand,
//                 } => {
//                     matches!(other,
//                         Expression::BinaryOperation {
//                             left_operand: other_left_operand,
//                             operator: other_operator, right_operand: other_right_operand
//                         }
//                         if **left_operand == **other_left_operand && operator == other_operator.kind() && **right_operand == **other_right_operand
//                     )
//                 }
//                 Self::UnaryOperation { operator, operand } => {
//                     matches!(other,
//                         Expression::UnaryOperation {
//                             operator: other_operator,
//                             operand: other_operand
//                         }
//                         if operator == other_operator.kind() && **operand == **other_operand
//                     )
//                 }
//             }
//         }
//     }
// }
