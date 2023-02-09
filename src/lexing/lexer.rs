use crate::diagnostics::{DiagnosticList, LexerDiagnosticKind};

use super::{cursor::Cursor, token::Token, token_kind::TokenKind, token_span::TokenSpan};

pub struct Lexer<'a> {
    source: &'a str,
    cursor: Cursor<'a>,
    diagnostics: DiagnosticList<LexerDiagnosticKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: Cursor::new(source),
            diagnostics: DiagnosticList::new(),
        }
    }

    pub fn diagnostics(&self) -> &DiagnosticList<LexerDiagnosticKind> {
        &self.diagnostics
    }

    pub fn lex(&mut self) -> Token<'a> {
        let Some(c) = self.cursor.peek() else {
            return Token::new(TokenKind::Eof, TokenSpan::empty(self.source.len()));
        };

        if c.is_whitespace() {
            self.cursor.consume();
            return self.lex();
        }

        if let Some(tok) = self.try_lex_identifier_or_keyword() {
            return tok;
        }

        if let Some(tok) = self.try_lex_number() {
            return tok;
        }

        if let Some(tok) = self.try_lex_string() {
            return tok;
        }

        let start_index = self.cursor.offset();

        self.cursor.consume();

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
                let span = TokenSpan::new(start_index, self.cursor.offset());
                self.diagnostics
                    .push_kind(LexerDiagnosticKind::IllegalCharacter(c), span);
                return self.lex();
            }
        };

        let span = TokenSpan::new(start_index, self.cursor.offset());

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

    fn try_read_word(&mut self) -> Option<TokenSpan> {
        let start_index = self.cursor.offset();

        let mut peek_iter = self.cursor.peek_iter();
        if !matches!(peek_iter.next(), Some(c) if Self::is_identifier_start(c)) {
            return None;
        }

        while matches!(peek_iter.next(), Some(c) if Self::is_identifier_continue(c)) {}

        Some(TokenSpan::new(start_index, self.cursor.offset()))
    }

    fn try_lex_identifier_or_keyword(&mut self) -> Option<Token<'a>> {
        let Some(span) = self.try_read_word() else {
            return None;
        };

        let text = span.slice(self.source);

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

    fn try_lex_number(&mut self) -> Option<Token<'a>> {
        let start_index = self.cursor.offset();

        let mut peek_iter = self.cursor.peek_iter();

        if !matches!(peek_iter.next(), Some(c) if c.is_ascii_digit()) {
            return None;
        }

        for next_char in peek_iter {
            if !next_char.is_ascii_digit() {
                break;
            }
        }

        // NOTE: could use read word for special syntax
        let has_word = self.try_read_word().is_some();

        let span = TokenSpan::new(start_index, self.cursor.offset());
        let text = span.slice(self.source);

        let kind = if has_word {
            self.diagnostics
                .push_kind(LexerDiagnosticKind::InvalidIntegerTrailingWord, span);
            TokenKind::MalformedInt
        } else {
            match text.parse() {
                Ok(num) => TokenKind::Int(num),
                Err(_) => {
                    self.diagnostics
                        .push_kind(LexerDiagnosticKind::InvalidIntegerTooLarge, span);
                    TokenKind::MalformedInt
                }
            }
        };

        Some(Token::new(kind, span))
    }

    fn try_lex_string(&mut self) -> Option<Token<'a>> {
        let start_index = self.cursor.offset();

        let Some('"') = self.cursor.peek() else {
            return None;
        };
        self.cursor.consume();

        // This will be `None` at the end of the loop if the string is malformed.
        let mut string = Some(String::new());

        // Start of the string data.
        let mut push_start = start_index + 1;

        'outer_loop: loop {
            // We first loop through all of the characters that are neither '"' nor '\\'.
            loop {
                match self.cursor.next() {
                    Some(next_char) => {
                        // End of string
                        if next_char == '"' {
                            // Write all the characters that we looped through if the string is not malformed and it is not empty.
                            // Because if it is empty, which means there are no escape sequences,
                            // we will just store a reference to `self.source` instead of allocating a `String`.
                            if let Some(ref mut string) = string {
                                if !string.is_empty() {
                                    // `self.cursor.offset() - 1` because we don't want to include the '"'
                                    string.push_str(
                                        &self.source[push_start..self.cursor.offset() - 1],
                                    );
                                }
                            }
                            break 'outer_loop;
                        }
                        // Escape sequence!!
                        if next_char == '\\' {
                            break;
                        }
                    }
                    None => {
                        string = None;
                        break 'outer_loop;
                    }
                }
            }

            // Write all the characters that we looped through if the string is not malformed.
            if let Some(ref mut string) = string {
                // `self.cursor.offset() - 1` because we don't want to include the '\\'
                string.push_str(&self.source[push_start..self.cursor.offset() - 1]);
            }

            // Try to parse the escape sequence.
            let Some(c) = self.try_parse_string_escape_sequence() else {
                // The string is malformed.
                string = None;
                continue 'outer_loop
            };

            // If the string is not malformed, push the escape sequence result.
            if let Some(ref mut string) = string {
                string.push(c);
                // The next portion of string we will loop through starts at `self.cursor.offset()`
                push_start = self.cursor.offset();
            }
        }

        let kind = if let Some(string) = string {
            if string.is_empty() {
                // No escape sequences, so no need to allocate memory
                TokenKind::String(self.source[start_index + 1..self.cursor.offset() - 1].into())
            } else {
                TokenKind::String(string.into())
            }
        } else {
            // HELP: is this an error? if so, push an error.
            TokenKind::MalformedString
        };
        let span = TokenSpan::new(start_index, self.cursor.offset());

        Some(Token::new(kind, span))
    }

    fn try_parse_string_escape_sequence(&mut self) -> Option<char> {
        Some(match self.cursor.next()? {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '0' => '\0',
            '"' => '"',
            'x' => self.try_parse_ascii_escape_sequence()?,
            'u' => self.try_parse_unicode_escape_sequence()?,
            c => {
                let offset = self.cursor.offset();
                let span = TokenSpan::new(offset, offset + 1);
                self.diagnostics
                    .push_kind(LexerDiagnosticKind::InvalidEscapeCharacter(c), span);
                return None;
            }
        })
    }

    fn try_parse_ascii_escape_sequence(&mut self) -> Option<char> {
        let offset = self.cursor.offset();
        let (first, second) = (self.cursor.next()?, self.cursor.next()?);

        let first = match first.to_digit(16) {
            None => {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::ExpectAsciiCharacterFirst(first),
                    TokenSpan::new(offset, offset + 1),
                );
                return None;
            }
            // an ascii character is 7 bits long in UTF-8, so the first byte must not exceed a value of 0x7.
            Some(n) if n > 0x7 => {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::InvalidAsciiCharacterCode(first, second),
                    TokenSpan::new(offset, offset + 2),
                );
                return None;
            }
            Some(n) => n as u8,
        };

        let second = match second.to_digit(16) {
            None => {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::ExpectAsciiCharacterSecond(second),
                    TokenSpan::new(offset + 1, offset + 2),
                );
                return None;
            }
            Some(n) => n as u8,
        };

        Some(((first << 4) | second) as char)
    }

    fn try_parse_unicode_escape_sequence(&mut self) -> Option<char> {
        let brace = self.cursor.next()?;
        let code_index = self.cursor.offset();

        if brace != '{' {
            self.diagnostics.push_kind(
                LexerDiagnosticKind::InvalidUnicodeSequenceMissingLeftBrace,
                TokenSpan::new(code_index - 2, code_index - 1),
            );
            return None;
        }

        let mut unicode = 0;
        let mut raw_unicode = String::new(); // needed for diagnostic message

        // get all the digits inside the braces
        for (i, next_char) in self.cursor.by_ref().enumerate() {
            if next_char == '}' {
                match char::from_u32(unicode) {
                    Some(c) => return Some(c),
                    None => {
                        let span = TokenSpan::new(code_index, code_index + i);
                        self.diagnostics
                            .push_kind(LexerDiagnosticKind::InvalidUnicodeCharacterCode, span);
                        return None;
                    }
                };
            }

            if i > 5 {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::InvalidUnicodeTooLong,
                    TokenSpan::new(code_index, code_index + i + 1),
                );
                return None;
            }

            let digit = match next_char.to_digit(16) {
                None => {
                    let span = TokenSpan::new(code_index + i, code_index + i + 1);
                    self.diagnostics
                        .push_kind(LexerDiagnosticKind::InvalidUnicodeDigit(next_char), span);
                    return None;
                }
                Some(n) => n,
            };

            unicode = (unicode << 4) | digit;
            raw_unicode.push(next_char);
        }

        // eof
        None
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
    test_tokens!(test_integer_2048_malformed { "2048malformed" => TokenKind::MalformedInt });
    test_tokens!(test_string_empty { "\"\"" => TokenKind::String("".into()) });
    test_tokens!(test_string_this_is_a_string_literal { "\"this is a string literal\"" => TokenKind::String("this is a string literal".into()) });
    test_tokens!(test_string_escape_sequences { "\"hello\\nhi\\twhat\\ridk\\\\yes\\0or\\\"no\"" => TokenKind::String("hello\nhi\twhat\ridk\\yes\0or\"no".into()) });
    test_tokens!(test_string_ascii_unicode_escape { "\"\\x5E\\x6F\\x61\\u{102}\\u{12345}\\u{103456}\"" => TokenKind::String("\x5E\x6F\x61\u{102}\u{12345}\u{103456}".into()) });
    test_tokens!(test_string_malformed { "\"malformed" => TokenKind::MalformedString });
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
