use std::mem;

use crate::{
    diagnostics::DiagnosticKind,
    lexing::{token::Token, token_discr::TokenDiscr},
    DiagnosticList, Lexer,
};

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    diagnostics: DiagnosticList,
    lookahead: Token<'a>,
}

impl<'a> Parser<'a> {
    // parser creation implies lexing the first token.
    // should this behavior be made explicit, i.e., in another function?
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
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

    pub fn diagnostics(&self) -> &DiagnosticList {
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
            self.diagnostics.push_kind(
                DiagnosticKind::UnexpectedToken { expected, found },
                self.lookahead.span(),
            );
            // we return a 'fake' token, because we aren't consuming the lookahead yet.
            // TODO: return a good 'fake' token, not eof
            Token::EOF // placeholder
        }
    }

    pub fn parse_empty_function(&mut self) {
        self.expect(TokenDiscr::KeywordFn);
        self.expect(TokenDiscr::Identifier);
        self.expect(TokenDiscr::LeftParen);
        self.expect(TokenDiscr::RightParen);
        self.expect(TokenDiscr::LeftBrace);
        self.expect(TokenDiscr::RightBrace);
        self.expect(TokenDiscr::Eof);
    }
}
