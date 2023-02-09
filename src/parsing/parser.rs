use std::mem;

use crate::{DiagnosticList, Lexer, lexing::{token::Token, token_discr::TokenDiscr, token_kind::TokenKind}};

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

    fn consume(&mut self) -> Token<'a> {
        // replace the lookahead with the new token, but return its previous value.
        mem::replace(&mut self.lookahead, self.lexer.lex())
    }

    fn expect(&mut self, discr: TokenDiscr) -> Token<'a> {
        if self.lookahead.kind().discr() == discr {
            self.consume()
        } else {
            // TODO: push expected token error
            // we return a 'fake' token, because we aren't consuming the lookahead yet.
            // TODO: return a good 'fake' token, not eof
            println!("expected {:?}, found {:?}", discr, self.lookahead.kind());
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
