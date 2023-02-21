use crate::diagnostics::{DiagnosticList, LexerDiagnosticKind};

use super::{Cursor, Token, TokenKind, TokenSpan};

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
        // do-while loop
        while {
            let peek_iter = self.cursor.peek_iter();

            // skip all the white spaces
            for c in peek_iter {
                if !c.is_whitespace() {
                    break;
                }
            }

            self.ignore_comment()
        } {}

        let Some(c) = self.cursor.peek() else {
            return Token::new(TokenKind::Eof, TokenSpan::empty(self.cursor.location()));
        };

        if let Some(tok) = self.try_lex_identifier_or_keyword() {
            return tok;
        }

        if let Some(tok) = self.try_lex_number() {
            return tok;
        }

        if let Some(tok) = self.try_lex_string() {
            return tok;
        }

        if let Some(tok) = self.try_lex_character() {
            return tok;
        }

        let start_location = self.cursor.location();

        self.cursor.consume();

        // TODO: 2-lengthed symbols
        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,

            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,

            '=' | '!' | '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' => {
                if let Some('=') = self.cursor.peek() {
                    self.cursor.consume();
                    match c {
                        '=' => TokenKind::EqualEqual,
                        '!' => TokenKind::BangEqual,
                        '+' => TokenKind::PlusEqual,
                        '-' => TokenKind::MinusEqual,
                        '*' => TokenKind::AsteriskEqual,
                        '/' => TokenKind::SlashEqual,
                        '&' => TokenKind::AmpersandEqual,
                        '|' => TokenKind::PipeEqual,
                        '<' => TokenKind::LessOrEqual,
                        '>' => TokenKind::GreaterOrEqual,
                        _ => unreachable!(),
                    }
                } else {
                    match c {
                        '=' => TokenKind::Equal,
                        '!' => TokenKind::Bang,
                        '+' => {
                            if let Some('+') = self.cursor.peek() {
                                self.cursor.consume();
                                TokenKind::PlusPlus
                            } else {
                                TokenKind::Plus
                            }
                        }
                        '-' => {
                            if let Some('-') = self.cursor.peek() {
                                self.cursor.consume();
                                TokenKind::MinusMinus
                            } else {
                                TokenKind::Minus
                            }
                        }
                        '*' => TokenKind::Asterisk,
                        '/' => TokenKind::Slash,
                        '&' => TokenKind::Ampersand,
                        '|' => TokenKind::Pipe,
                        '<' => {
                            if let Some('<') = self.cursor.peek() {
                                self.cursor.consume();
                                TokenKind::LeftShift
                            } else {
                                TokenKind::LeftChevron
                            }
                        }
                        '>' => {
                            if let Some('>') = self.cursor.peek() {
                                self.cursor.consume();
                                TokenKind::RightShift
                            } else {
                                TokenKind::RightChevron
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }

            _ => {
                let span = TokenSpan::new(start_location, self.cursor.location());
                self.diagnostics
                    .push_kind(LexerDiagnosticKind::IllegalCharacter(c), span);
                return self.lex();
            }
        };

        let span = TokenSpan::new(start_location, self.cursor.location());

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
        let start_location = self.cursor.location();

        let mut peek_iter = self.cursor.peek_iter();
        if !matches!(peek_iter.next(), Some(c) if Self::is_identifier_start(c)) {
            return None;
        }

        while matches!(peek_iter.next(), Some(c) if Self::is_identifier_continue(c)) {}

        Some(TokenSpan::new(start_location, self.cursor.location()))
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
        let start_location = self.cursor.location();

        let mut cursor = self.cursor.clone();

        let Some(mut peek) = cursor.peek() else {
            return None;
        };

        let mut is_float = false;

        if peek == '.' {
            cursor.consume();

            let Some(peek_after) = cursor.peek() else {
                return None;
            };

            peek = peek_after;

            is_float = true;
        }

        if !peek.is_ascii_digit() {
            return None;
        }

        self.cursor = cursor;

        let peek_iter = self.cursor.peek_iter();

        let mut ends_with_dot = false;

        for next_char in peek_iter {
            ends_with_dot = false;

            if !next_char.is_ascii_digit() {
                if !is_float && next_char == '.' {
                    is_float = true;
                    ends_with_dot = true;
                    continue;
                }
                break;
            }
        }

        // NOTE: could use read word for special syntax
        let has_word = self.try_read_word().is_some();

        let span = TokenSpan::new(start_location, self.cursor.location());
        let text = span.slice(self.source);

        if ends_with_dot {
            return Some(Token::new(TokenKind::MalformedFloat, span));
        }

        #[allow(clippy::collapsible_else_if)]
        let kind = if !is_float {
            if has_word {
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
            }
        } else {
            if has_word {
                self.diagnostics
                    .push_kind(LexerDiagnosticKind::InvalidFloatTrailingWord, span);
                TokenKind::MalformedFloat
            } else {
                match text.parse() {
                    Ok(num) => TokenKind::Float(num),
                    Err(_) => {
                        self.diagnostics
                            .push_kind(LexerDiagnosticKind::InvalidFloat, span);
                        TokenKind::MalformedFloat
                    }
                }
            }
        };

        Some(Token::new(kind, span))
    }

    fn try_lex_string(&mut self) -> Option<Token<'a>> {
        let start_location = self.cursor.location();

        let Some('"') = self.cursor.peek() else {
            return None;
        };
        self.cursor.consume();

        // This will be `None` at the end of the loop if the string is malformed.
        let mut string = Some(String::new());

        // Start of the string data.
        let mut push_start = self.cursor.location();

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
                                        &self.source
                                            [push_start.byte()..self.cursor.location().byte() - 1],
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
                        self.diagnostics.push_kind(
                            LexerDiagnosticKind::UnclosedString,
                            TokenSpan::new(start_location, self.cursor.location()),
                        );

                        string = None;
                        break 'outer_loop;
                    }
                }
            }

            // Write all the characters that we looped through if the string is not malformed.
            if let Some(ref mut string) = string {
                // `self.cursor.offset() - 1` because we don't want to include the '\\'
                string.push_str(&self.source[push_start.byte()..self.cursor.location().byte() - 1]);
            }

            // Try to parse the escape sequence.
            let Some(c) = self.try_parse_escape_sequence() else {
                // The string is malformed.
                string = None;
                continue 'outer_loop
            };

            // If the string is not malformed, push the escape sequence result.
            if let Some(ref mut string) = string {
                string.push(c);
                // The next portion of string we will loop through starts at `self.cursor.offset()`
                push_start = self.cursor.location();
            }
        }

        let kind = if let Some(string) = string {
            if string.is_empty() {
                // No escape sequences, so no need to allocate memory
                TokenKind::String(
                    self.source[start_location.byte() + 1..self.cursor.location().byte() - 1]
                        .into(),
                )
            } else {
                TokenKind::String(string.into())
            }
        } else {
            // HELP: is this an error? if so, push an error.
            TokenKind::MalformedString
        };
        let span = TokenSpan::new(start_location, self.cursor.location());

        Some(Token::new(kind, span))
    }

    fn try_lex_character(&mut self) -> Option<Token<'a>> {
        let start_location = self.cursor.location();

        let Some('\'') = self.cursor.peek() else {
            return None;
        };
        self.cursor.consume();

        let c = {
            let Some(c) = self.cursor.next() else {
                return Some(Token::new(TokenKind::MalformedCharacter, TokenSpan::new(
                        start_location,
                        self.cursor.location(),
                    )
                ))
            };
            if c == '\\' {
                let Some(c) = self.try_parse_escape_sequence() else {
                    return Some(Token::new(TokenKind::MalformedCharacter, TokenSpan::new(
                            start_location,
                            self.cursor.location(),
                        )
                    ))
                };
                c
            } else {
                c
            }
        };

        Some(Token::new(
            if let Some('\'') = self.cursor.next() {
                TokenKind::Character(c)
            } else {
                TokenKind::MalformedCharacter
            },
            TokenSpan::new(start_location, self.cursor.location()),
        ))
    }

    fn try_parse_escape_sequence(&mut self) -> Option<char> {
        Some(match self.cursor.next()? {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '0' => '\0',
            '"' => '"',
            '\'' => '\'',
            'x' => self.try_parse_ascii_escape_sequence()?,
            'u' => self.try_parse_unicode_escape_sequence()?,
            c => {
                let location = self.cursor.location();
                let span = TokenSpan::new(location, location + 1);
                self.diagnostics
                    .push_kind(LexerDiagnosticKind::InvalidEscapeCharacter(c), span);
                return None;
            }
        })
    }

    fn try_parse_ascii_escape_sequence(&mut self) -> Option<char> {
        let first_location = self.cursor.location();
        let first = self.cursor.next()?;

        let second_location = self.cursor.location();
        let second = self.cursor.next()?;

        let last_location = self.cursor.location();

        let first = match first.to_digit(16) {
            None => {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::ExpectAsciiCharacterFirst(first),
                    TokenSpan::new(first_location, second_location),
                );
                return None;
            }
            // an ascii character is 7 bits long in UTF-8, so the first byte must not exceed a value of 0x7.
            Some(n) if n > 0x7 => {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::InvalidAsciiCharacterCode(first, second),
                    TokenSpan::new(first_location, last_location),
                );
                return None;
            }
            Some(n) => n as u8,
        };

        let second = match second.to_digit(16) {
            None => {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::ExpectAsciiCharacterSecond(second),
                    TokenSpan::new(second_location, last_location),
                );
                return None;
            }
            Some(n) => n as u8,
        };

        Some(((first << 4) | second) as char)
    }

    fn try_parse_unicode_escape_sequence(&mut self) -> Option<char> {
        let start_index = self.cursor.location();

        let brace = self.cursor.next()?;
        let code_index = self.cursor.location();

        if brace != '{' {
            self.diagnostics.push_kind(
                LexerDiagnosticKind::InvalidUnicodeSequenceMissingLeftBrace,
                TokenSpan::new(start_index, code_index),
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
                        let span = TokenSpan::new(code_index, self.cursor.location() - 1);
                        self.diagnostics
                            .push_kind(LexerDiagnosticKind::InvalidUnicodeCharacterCode, span);
                        return None;
                    }
                };
            }

            if i > 5 {
                self.diagnostics.push_kind(
                    LexerDiagnosticKind::InvalidUnicodeTooLong,
                    TokenSpan::new(code_index, self.cursor.location()),
                );
                return None;
            }

            let digit = match next_char.to_digit(16) {
                None => {
                    let span = TokenSpan::new(self.cursor.location() - 1, self.cursor.location());
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

    fn ignore_comment(&mut self) -> bool {
        let start_location = self.cursor.location();

        let mut cursor = self.cursor.clone();

        let Some('/') = cursor.next() else {
            return false;
        };

        match cursor.next() {
            // "// line comment"
            Some('/') => {
                self.cursor = cursor;

                let mut peek_iter = self.cursor.peek_iter();

                while !matches!(peek_iter.next(), None | Some('\n')) {}

                true
            }
            // "/* block comment */"
            Some('*') => {
                self.cursor = cursor;

                let mut peek_iter = self.cursor.peek_iter();

                while let Some(c) = peek_iter.next() {
                    if c == '*' {
                        if let Some('/') = peek_iter.next() {
                            self.cursor.consume();

                            return true;
                        }
                    }
                }

                self.diagnostics.push_kind(
                    LexerDiagnosticKind::UnclosedBlockComment,
                    TokenSpan::new(start_location, self.cursor.location()),
                );

                true
            }

            None | Some(_) => false,
        }
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
    test_tokens!(test_float_2_5 { "2.5" => TokenKind::Float(2.5) });
    test_tokens!(test_float_0_2 { ".2" => TokenKind::Float(0.2) });
    test_tokens!(test_float_2_0 { "2." => TokenKind::MalformedFloat });
    test_tokens!(test_float_20_48_malformed { "20.48malformed" => TokenKind::MalformedFloat });
    test_tokens!(test_string_empty { "\"\"" => TokenKind::String("".into()) });
    test_tokens!(test_string_this_is_a_string_literal { "\"this is a string literal\"" => TokenKind::String("this is a string literal".into()) });
    test_tokens!(test_string_escape_sequences { "\"hello\\nhi\\twhat\\ridk\\\\yes\\0or\\\"no\"" => TokenKind::String("hello\nhi\twhat\ridk\\yes\0or\"no".into()) });
    test_tokens!(test_string_ascii_unicode_escape { "\"\\x5E\\x6F\\x61\\u{102}\\u{12345}\\u{103456}\"" => TokenKind::String("\x5E\x6F\x61\u{102}\u{12345}\u{103456}".into()) });
    test_tokens!(test_string_malformed { "\"malformed" => TokenKind::MalformedString });
    test_tokens!(test_character_a { "'a'" => TokenKind::Character('a') });
    test_tokens!(test_character_b { "'b'" => TokenKind::Character('b') });
    test_tokens!(test_character_ascii_escape { "'\\x5E'" => TokenKind::Character('\x5E') });
    test_tokens!(test_character_unicode_escape { "'\\u{102}'" => TokenKind::Character('\u{102}') });
    test_tokens!(test_character_malformed { "'m" => TokenKind::MalformedCharacter });
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
    test_tokens!(test_plus { "+" => TokenKind::Plus });
    test_tokens!(test_minus { "-" => TokenKind::Minus });
    test_tokens!(test_asterisk { "*" => TokenKind::Asterisk });
    test_tokens!(test_slash { "/" => TokenKind::Slash });
    test_tokens!(test_ampersand { "&" => TokenKind::Ampersand });
    test_tokens!(test_pipe { "|" => TokenKind::Pipe });
    test_tokens!(test_equal { "=" => TokenKind::Equal });
    test_tokens!(test_plus_equal { "+=" => TokenKind::PlusEqual });
    test_tokens!(test_minus_equal { "-=" => TokenKind::MinusEqual });
    test_tokens!(test_asterisk_equal { "*=" => TokenKind::AsteriskEqual });
    test_tokens!(test_slash_equal { "/=" => TokenKind::SlashEqual });
    test_tokens!(test_ampersand_equal { "&=" => TokenKind::AmpersandEqual });
    test_tokens!(test_pipe_equal { "|=" => TokenKind::PipeEqual });
    test_tokens!(test_equal_equal { "==" => TokenKind::EqualEqual });
    test_tokens!(test_not_equal { "!=" => TokenKind::BangEqual });
    test_tokens!(test_less_or_equal { "<=" => TokenKind::LessOrEqual });
    test_tokens!(test_greater_or_equal { ">=" => TokenKind::GreaterOrEqual });
    test_tokens!(test_left_shift { "<<" => TokenKind::LeftShift });
    test_tokens!(test_right_shift { ">>" => TokenKind::RightShift });
    test_tokens!(test_not { "!" => TokenKind::Bang });
    test_tokens!(test_plus_plus { "++" => TokenKind::PlusPlus });
    test_tokens!(test_minus_minus { "--" => TokenKind::MinusMinus });
    test_tokens!(test_line_comment { "// hello how are you ?\n" => TokenKind::Eof });
    test_tokens!(test_line_comment_eof { "// hello how are you ?" => TokenKind::Eof });
    test_tokens!(test_block_comment { "/* hello how are you ? */" => TokenKind::Eof });
    test_tokens!(test_block_comment_lf { "/* hello how\nare you ? */" => TokenKind::Eof });
    test_tokens!(test_block_comment_malformed { "/* hello how\nare you ?* / *" => TokenKind::Eof });
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
