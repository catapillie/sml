use std::mem;

use crate::{
    diagnostics::{LexerDiagnosticKind, ParserDiagnosticKind},
    lexing::{token::Token, token_discr::TokenDiscr, token_kind::TokenKind, token_span::TokenSpan},
    DiagnosticList, Lexer,
};

use super::{
    expression::Expression,
    operator::{BinaryOperator, PreUnaryOperator},
    parser_ast::ParserAST,
    priority::Priority,
    statement::Statement,
};

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
        if let Some(left_brace) = self.try_expect(TokenDiscr::LeftBrace) {
            let mut statements = Vec::new();

            while !matches!(self.lookahead.discr(), TokenDiscr::RightBrace | TokenDiscr::Eof) {
                statements.push(self.parse_statement());
            }

            let right_brace = self.expect(TokenDiscr::RightBrace);

            return Statement::Block {
                left_brace,
                statements,
                right_brace,
            };
        }

        Statement::None
    }

    pub fn parse_expression(&mut self) -> Expression<'a> {
        self.parse_operation_expression(Priority::default())
    }

    fn parse_operation_expression(&mut self, priority: Priority) -> Expression<'a> {
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

    fn parse_primary_expression(&mut self) -> Expression<'a> {
        match self.lookahead.discr() {
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
