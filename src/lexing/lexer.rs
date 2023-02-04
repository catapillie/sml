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

    pub fn lex(&mut self) -> Token<'a> {
        let mut chars = self.source.chars();
        let c = match chars.nth(self.cursor) {
            Some(c) => c,
            None => return Token::EOF,
        };

        if c.is_whitespace() {
            self.cursor += 1;
            return self.lex();
        }

        if let Some(tok) = self.try_lex_identifier_or_keyword(c, &mut chars) {
            return tok;
        }

        if let Some(tok) = self.try_lex_number(c, &mut chars) {
            return tok;
        }

        let span = TokenSpan::new(self.cursor, self.cursor + 1);
        self.cursor += 1;

        // TODO: 2-lengthed symbols
        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
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

    pub fn lex_all(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let tok = self.lex();
            let is_eof = *tok.kind() == TokenKind::Eof;

            tokens.push(tok);

            if is_eof {
                break;
            }
        }

        tokens
    }

    fn try_read_word(&mut self, c: char, chars: &mut Chars) -> Option<(&'a str, TokenSpan)> {
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

        let text = &self.source[start_index..self.cursor];
        let span = TokenSpan::new(start_index, self.cursor);

        Some((text, span))
    }

    fn try_lex_identifier_or_keyword(&mut self, c: char, chars: &mut Chars) -> Option<Token<'a>> {
        let Some((text, span)) = self.try_read_word(c, chars) else {
            return None;
        };

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

    // TODO: read alphabetic characters that follow integer literal, and return MalformedInt
    fn try_lex_number(&mut self, mut c: char, chars: &mut Chars) -> Option<Token<'a>> {
        if !c.is_ascii_digit() {
            return None;
        }

        let start_index = self.cursor;
        self.cursor += 1;

        for next_char in &mut *chars {
            if !next_char.is_ascii_digit() {
                c = next_char;
                break;
            }
            self.cursor += 1;
        }

        // NOTE: could use read word for special syntax
        let has_word = self.try_read_word(c, chars).is_some();

        let text = &self.source[start_index..self.cursor];

        let span = TokenSpan::new(start_index, self.cursor);
        let kind = if has_word {
            // TODO: push invalid integer literal error
            TokenKind::MalformedInt(text)
        } else {
            match text.parse() {
                Ok(num) => TokenKind::Int(num),
                Err(_) => {
                    // TODO: push invalid integer literal error
                    TokenKind::MalformedInt(text)
                }
            }
        };

        Some(Token::new(kind, span))
    }
}
