use std::str::Chars;

use super::{token::Token, token_kind::TokenKind, token_span::TokenSpan};

pub struct Lexer<'a> {
    source: &'a str,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, cursor: 0 }
    }

    pub fn lex(&mut self) -> Token {
        let mut chars = self.source.chars();
        let c = match chars.nth(self.cursor) {
            Some(c) => c,
            None => return Token::EOF,
        };

        if let Some(tok) = self.try_lex_identifier_or_keyword(c, &mut chars) {
            return tok;
        }

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

    fn try_lex_identifier_or_keyword(&mut self, c: char, chars: &mut Chars) -> Option<Token<'a>> {
        if !Self::is_identifier_start(c) {
            return None;
        }

        let start_index = self.cursor;
        self.cursor += 1;

        for next_char in chars {
            if !Self::is_identifier_continue(next_char) {
                break;
            }
            self.cursor += 1;
        }

        let span = TokenSpan::new(start_index, self.cursor);
        let text = &self.source[start_index..self.cursor];

        let kind = match text {
            "fn" => TokenKind::KeywordFn,
            "if" => TokenKind::KeywordIf,
            "unless" => TokenKind::KeywordUnless,
            "while" => TokenKind::KeywordWhile,
            "until" => TokenKind::KeywordUntil,
            "else" => TokenKind::KeywordElse,
            "forever" => TokenKind::KeywordForever,
            "repeat" => TokenKind::KeywordRepeat,
            _ => TokenKind::Identifier(text),
        };

        Some(Token::new(kind, span))
    }

    fn is_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_identifier_continue(c: char) -> bool {
        Self::is_identifier_start(c) || c.is_ascii_digit()
    }
}
