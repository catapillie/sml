use super::{token::Token, token_span::TokenSpan, token_kind::TokenKind};

pub struct Lexer<'a> {
    source: &'a str,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, cursor: 0 }
    }

    pub fn lex(&mut self) -> Token {
        let c = match self.source.chars().nth(self.cursor) {
            Some(c) => c,
            None => return Token::EOF,
        };

        let span = TokenSpan::new(self.cursor, self.cursor + 1);
        self.cursor += 1;

        // TODO: 2-lengthed symbols
        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBrace,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '<' => TokenKind::LeftChevron,
            '>' => TokenKind::RightChevron,

            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,

            '=' => TokenKind::Equal,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Pipe,

            _ => {
                // TODO: push illegal character error
                return self.lex();
            }
        };

        Token::new(kind, span)
    }
}
