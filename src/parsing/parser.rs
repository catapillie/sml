use std::mem;

use crate::{
    lexing::{token::Token, token_discr::TokenDiscr, token_kind::TokenKind, token_span::TokenSpan},
    DiagnosticList, Lexer, diagnostics::ParserDiagnosticKind,
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
        if let Some(tok) = self.try_expect(expected) {
            tok
        } else {
            self.diagnostics.push_kind(
                ParserDiagnosticKind::UnexpectedToken {
                    expected,
                    found: self.lookahead.kind().discr(),
                },
                self.lookahead.span(),
            );
            // we return a 'fake' token, because we aren't consuming the lookahead yet.
            let span = TokenSpan::empty(self.lookahead.span().start());
            Token::new(TokenKind::Fake(expected), span)
        }
    }

    fn try_expect(&mut self, expected: TokenDiscr) -> Option<Token<'a>> {
        if self.lookahead.kind().discr() == expected {
            Some(self.consume())
        } else {
            None
        }
    }

    //pub fn parse_empty_function(&mut self) {
    //    self.expect(TokenDiscr::KeywordFn);
    //    self.expect(TokenDiscr::Identifier);
    //    self.expect(TokenDiscr::LeftParen);
    //    self.expect(TokenDiscr::RightParen);
    //    self.expect(TokenDiscr::LeftBrace);
    //    self.expect(TokenDiscr::RightBrace);
    //    self.expect(TokenDiscr::Eof);
    //}

    pub fn parse_three_primary_expressions(&mut self) {
        self.parse_primary_expression();
        self.parse_primary_expression();
        self.parse_primary_expression();
    }

    // TODO: return AST node
    fn parse_primary_expression(&mut self) {
        if let Some(tok) = self.try_expect(TokenDiscr::Int) {
            // TODO: return integer literal node
        } else if let Some(tok) = self.try_expect(TokenDiscr::String) {
            // TODO: return string literal node
        } else if let Some(tok) = self.try_expect(TokenDiscr::Identifier) {
            // TODO: return identifier node
        } else {
            // no primary expression is found, so let's push an error.
            self.diagnostics.push_kind(ParserDiagnosticKind::ExpectedExpression, self.lookahead.span());
        }
    }
}
