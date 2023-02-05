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

        if let Some(tok) = self.try_lex_string(c, &mut chars) {
            return tok;
        }

        let start_index = self.cursor;
        self.cursor += 1;

        // TODO: 2-lengthed symbols
        // TODO: escaped characters in string literal
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

        let span = TokenSpan::new(start_index, self.cursor);

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

    fn try_lex_string(&mut self, c: char, chars: &mut Chars) -> Option<Token<'a>> {
        if c != '"' {
            return None;
        }

        let start_index = self.cursor;

        self.cursor += 1;

        let mut string = String::new();
        let mut closed = false;

        'outer_loop: while let Some(next_char) = chars.next() {
            self.cursor += 1;

            if next_char == '"' {
                closed = true;
                break;
            }
            if next_char != '\\' {
                string.push(next_char);
                continue;
            }
            self.cursor += 1;

            let Some(c) = chars.next() else {
                break;
            };

            string.push(match c {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '0' => '\0',
                '"' => '"',
                'x' => {
                    let Some(first) = chars.next() else {
                        break;
                    };
                    self.cursor += 1;
                    let Some(second) = chars.next() else {
                        break;
                    };
                    self.cursor += 1;

                    let first = match first.to_digit(16) {
                        None => {
                            // TODO: push invalid character in ASCII escape sequence error
                            0 // replacement value
                        }
                        Some(n) => {
                            if n > 0x7 {
                                // TODO: push too big error
                                0 // replacement value
                            } else {
                                n as u8
                            }
                        }
                    };

                    // TODO: push invalid character in ASCII escape sequence error when `to_digit` returns `None`
                    let second = second.to_digit(16).unwrap_or(0) as u8;

                    ((first << 4) | second) as char
                }
                'u' => {
                    let Some(brace) = chars.next() else {
                        break;
                    };
                    self.cursor += 1;

                    if brace != '{' {
                        // TODO: push invalid character in unicode escape sequence error
                        continue;
                    }

                    let mut unicode = 0;
                    let mut i = 0;

                    // get all the digits inside the braces
                    loop {
                        match chars.next() {
                            Some(digit) => {
                                self.cursor += 1;
                                // end of the escape sequence
                                if digit == '}' {
                                    // TODO: push invalid unicode on `None`
                                    match char::from_u32(unicode) {
                                        Some(c) => break c,
                                        None => continue 'outer_loop,
                                    };
                                }

                                // too long
                                if i > 5 {
                                    // TODO: push unicode too long
                                    continue 'outer_loop;
                                }

                                // TODO: push invalid character in unicode escape on `None`
                                let digit = digit.to_digit(16).unwrap_or(0) as u8;

                                unicode = (unicode << 4) | digit as u32;

                                i += 1;
                            }
                            None => continue 'outer_loop,
                        }
                    }
                }
                c => {
                    // TODO: push unknown escape sequence error
                    c
                }
            });
        }

        let kind = if closed {
            TokenKind::String(string)
        } else {
            dbg!(
                "start: {}\ncursor: {}\nsource: {}",
                start_index,
                self.cursor,
                self.source
            );
            TokenKind::MalformedString(&self.source[(start_index)..(self.cursor)])
        };
        let span = TokenSpan::new(start_index, self.cursor);

        Some(Token::new(kind, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_tokens {
        ($name:ident { $($raw:literal => $kind:expr)* }) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new(concat!($(" ", $raw),*));

                let tokens = lexer.lex_all();

                let expected_kinds = [$($kind),*];

                for (token, expected) in tokens.iter().zip(expected_kinds.iter()) {
                    assert_eq!(token.kind(), expected);
                }

                // check if the length is different, ignoring any trailing EOFs in the output tokens.
                if tokens.iter().skip(expected_kinds.len()).any(|token| token.kind() != &TokenKind::Eof) {
                    panic!("expected {} tokens (+ EOF), but got {}: {tokens:?}", expected_kinds.len(), tokens.len());
                }
            }
        };
    }

    test_tokens!(test_keyword_fn { "fn" => TokenKind::KeywordFn });
    test_tokens!(test_keyword_if { "if" => TokenKind::KeywordIf });
    test_tokens!(test_keyword_unless { "unless" => TokenKind::KeywordUnless });
    test_tokens!(test_keyword_while { "while" => TokenKind::KeywordWhile });
    test_tokens!(test_keyword_until { "until" => TokenKind::KeywordUntil });
    test_tokens!(test_keyword_else { "else" => TokenKind::KeywordElse });
    test_tokens!(test_keyword_forever { "forever" => TokenKind::KeywordForever });
    test_tokens!(test_keyword_repeat { "repeat" => TokenKind::KeywordRepeat });
    test_tokens!(test_identifier_abc { "abc" => TokenKind::Identifier("abc") });
    test_tokens!(test_identifier_abclonger { "abclonger" => TokenKind::Identifier("abclonger") });
    test_tokens!(test_identifier_main { "main" => TokenKind::Identifier("main") });
    test_tokens!(test_integer_0 { "0" => TokenKind::Int(0) });
    test_tokens!(test_integer_65536 { "65536" => TokenKind::Int(65536) });
    test_tokens!(test_integer_1 { "1" => TokenKind::Int(1) });
    test_tokens!(test_integer_2048_malformed { "2048malformed" => TokenKind::MalformedInt("2048malformed") });
    test_tokens!(test_string_empty { "\"\"" => TokenKind::String(String::from("")) });
    test_tokens!(test_string_this_is_a_string_literal { "\"this is a string literal\"" => TokenKind::String(String::from("this is a string literal")) });
    test_tokens!(test_string_escape_sequences { "\"hello\\nhi\\twhat\\ridk\\\\yes\\0or\\\"no\"" => TokenKind::String(String::from("hello\nhi\twhat\ridk\\yes\0or\"no")) });
    test_tokens!(test_string_ascii_unicode_escape { "\"\\x5E\\x6F\\x61\\u{102}\\u{12345}\\u{103456}\"" => TokenKind::String(String::from("\x5E\x6F\x61\u{102}\u{12345}\u{103456}")) });
    test_tokens!(test_string_malformed { "\"malformed" => TokenKind::MalformedString("\"malformed") });
    test_tokens!(test_left_paren { "(" => TokenKind::LeftParen });
    test_tokens!(test_right_paren { ")" => TokenKind::RightParen });
    test_tokens!(test_left_bracket { "[" => TokenKind::LeftBracket });
    test_tokens!(test_right_bracket { "]" => TokenKind::RightBracket });
    test_tokens!(test_left_brace { "{" => TokenKind::LeftBrace });
    test_tokens!(test_right_brace { "}" => TokenKind::RightBrace });
    test_tokens!(test_left_chevron { "<" => TokenKind::LeftChevron });
    test_tokens!(test_right_chevron { ">" => TokenKind::RightChevron });
    test_tokens!(test_dot { "." => TokenKind::Dot });
    test_tokens!(test_comma { "," => TokenKind::Comma });
    test_tokens!(test_colon { ":" => TokenKind::Colon });
    test_tokens!(test_semicolon { ";" => TokenKind::Semicolon });
    test_tokens!(test_equal { "=" => TokenKind::Equal });
    test_tokens!(test_plus { "+" => TokenKind::Plus });
    test_tokens!(test_minus { "-" => TokenKind::Minus });
    test_tokens!(test_asterisk { "*" => TokenKind::Asterisk });
    test_tokens!(test_slash { "/" => TokenKind::Slash });
    test_tokens!(test_ampersand { "&" => TokenKind::Ampersand });
    test_tokens!(test_pipe { "|" => TokenKind::Pipe });
    test_tokens!(test_eof { "" => TokenKind::Eof });

    test_tokens!(test_fn_main {
        "fn" => TokenKind::KeywordFn
        "main" => TokenKind::Identifier("main")
        "(" => TokenKind::LeftParen
        ")" => TokenKind::RightParen
        "{" => TokenKind::LeftBrace
        "}" => TokenKind::RightBrace
    });
}
