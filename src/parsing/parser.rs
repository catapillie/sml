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
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Expression<'a> {
        if let Some(token) = self.try_expect(TokenDiscr::Int) {
            Expression::Literal { token }
        } else if let Some(token) = self.try_expect(TokenDiscr::String) {
            Expression::Literal { token }
        } else if let Some(token) = self.try_expect(TokenDiscr::Identifier) {
            Expression::Literal { token }
        } else if let Some(left_paren) = self.try_expect(TokenDiscr::LeftParen) {
            let expression = Box::new(self.parse_expression());
            let right_paren = self.expect(TokenDiscr::RightParen);
            Expression::Parenthesized {
                left_paren,
                expression,
                right_paren,
            }
        } else {
            self.diagnostics.push_kind(
                ParserDiagnosticKind::ExpectedExpression,
                self.lookahead.span(),
            );
            Expression::None
        }
    }
}
