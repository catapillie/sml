use std::mem;

use crate::{
    diagnostics::ParserDiagnosticKind,
    lexing::{token::Token, token_discr::TokenDiscr, token_kind::TokenKind, token_span::TokenSpan},
    DiagnosticList, Lexer,
};

use super::{expression::Expression, parser_ast::ParserAST};

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

    // we (currently) need to access the lexer through the parser,
    // because we pass lexer's mutable reference to the parser.
    pub fn lexer(&self) -> &Lexer<'a> {
        &self.lexer
    }

    pub fn diagnostics(&self) -> &DiagnosticList<ParserDiagnosticKind> {
        &self.diagnostics
    }

    fn consume(&mut self) -> Token<'a> {
        // replace the lookahead with the new token, but return its previous value.
        mem::replace(&mut self.lookahead, self.lexer.lex())
    }

    fn expect(&mut self, expected: TokenDiscr) -> Token<'a> {
        let found = self.lookahead.kind().discr();
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
                            found: self.lookahead.kind().discr(),
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
        if self.lookahead.kind().discr() == expected {
            Some(self.consume())
        } else {
            None
        }
    }

    pub fn parse_expression(&mut self) -> Expression<'a> {
        self.parse_operation_expression(0)
    }

    fn parse_operation_expression(&mut self, precedence: u8) -> Expression<'a> {
        let mut left = if let Some(precedence_ahead) = self.unary_operator_precedence() {
            let operator = self.consume();
            Expression::UnaryOperation {
                operator,
                operand: Box::new(self.parse_operation_expression(precedence_ahead)),
            }
        } else {
            self.parse_primary_expression()
        };

        loop {
            // no operator ahead, break and return immediately the primary expression.
            let Some(precedence_ahead) = self.binary_operator_precedence() else {
                break;
            };

            // the operator located ahead has lower precedence.
            // we thus have to break early, return the current operator,
            // and let the previous call build the binary expression,
            // so that the tree accurately respects the order of operations that we defined.
            if precedence_ahead < precedence {
                break;
            }

            // we can parse the operation here.
            // consume the operator, parse next operation *with higher precedence*.
            // put the left expression as left operand.
            let operator = self.consume();
            let right = self.parse_operation_expression(precedence_ahead);
            left = Expression::BinaryOperation {
                left_operand: Box::new(left),
                operator,
                right_operand: Box::new(right),
            };
        }

        // finally return the built expression.
        left
    }

    // returns None if lookahead is not an operator.
    fn binary_operator_precedence(&self) -> Option<u8> {
        match self.lookahead.kind().discr() {
            TokenDiscr::Ampersand => Some(30),
            TokenDiscr::Pipe => Some(30),
            TokenDiscr::Asterisk => Some(30),
            TokenDiscr::Slash => Some(30),
            TokenDiscr::Plus => Some(20),
            TokenDiscr::Minus => Some(20),
            TokenDiscr::Equal => Some(10),
            _ => None,
        }
    }

    // returns None if lookahead is not a unary operator.
    fn unary_operator_precedence(&self) -> Option<u8> {
        match self.lookahead.kind().discr() {
            TokenDiscr::Plus => Some(50),
            TokenDiscr::Minus => Some(50),
            _ => None,
        }
    }

    fn parse_primary_expression(&mut self) -> Expression<'a> {
        match self.lookahead.kind().discr() {
            TokenDiscr::Int | TokenDiscr::String | TokenDiscr::Identifier => Expression::Literal {
                token: self.consume(),
            },
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
}
