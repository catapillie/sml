use crate::diagnostics::{DiagnosticList, LexerDiagnosticKind};

use super::{token::OnlySpanToken, Cursor, Token, TokenSpan};

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
            return Token::Eof;
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

        let kind = match c {
            '(' => OnlySpanToken::LeftParen,
            ')' => OnlySpanToken::RightParen,
            '[' => OnlySpanToken::LeftBracket,
            ']' => OnlySpanToken::RightBracket,
            '{' => OnlySpanToken::LeftBrace,
            '}' => OnlySpanToken::RightBrace,

            '.' => OnlySpanToken::Dot,
            ',' => OnlySpanToken::Comma,
            ':' => OnlySpanToken::Colon,
            ';' => OnlySpanToken::Semicolon,

            '=' | '!' | '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' => {
                if let Some('=') = self.cursor.peek() {
                    self.cursor.consume();
                    match c {
                        '=' => OnlySpanToken::EqualEqual,
                        '!' => OnlySpanToken::BangEqual,
                        '+' => OnlySpanToken::PlusEqual,
                        '-' => OnlySpanToken::MinusEqual,
                        '*' => OnlySpanToken::AsteriskEqual,
                        '/' => OnlySpanToken::SlashEqual,
                        '&' => OnlySpanToken::AmpersandEqual,
                        '|' => OnlySpanToken::PipeEqual,
                        '<' => OnlySpanToken::LessOrEqual,
                        '>' => OnlySpanToken::GreaterOrEqual,
                        _ => unreachable!(),
                    }
                } else {
                    match c {
                        '=' => OnlySpanToken::Equal,
                        '!' => OnlySpanToken::Bang,
                        '+' => {
                            if let Some('+') = self.cursor.peek() {
                                self.cursor.consume();
                                OnlySpanToken::PlusPlus
                            } else {
                                OnlySpanToken::Plus
                            }
                        }
                        '-' => {
                            if let Some('-') = self.cursor.peek() {
                                self.cursor.consume();
                                OnlySpanToken::MinusMinus
                            } else {
                                OnlySpanToken::Minus
                            }
                        }
                        '*' => OnlySpanToken::Asterisk,
                        '/' => OnlySpanToken::Slash,
                        '&' => OnlySpanToken::Ampersand,
                        '|' => OnlySpanToken::Pipe,
                        '<' => {
                            if let Some('<') = self.cursor.peek() {
                                self.cursor.consume();
                                OnlySpanToken::LeftShift
                            } else {
                                OnlySpanToken::LeftChevron
                            }
                        }
                        '>' => {
                            if let Some('>') = self.cursor.peek() {
                                self.cursor.consume();
                                OnlySpanToken::RightShift
                            } else {
                                OnlySpanToken::RightChevron
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

        kind(span).into()
    }

    pub fn lex_all(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let tok = self.lex();
            let is_eof = matches!(tok, Token::Eof(_));

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

    macro_rules! join {
        ($sep:literal; $first:literal $($other:literal)*) => {
            concat!($first, $($sep, $other),*)
        }
    }

    macro_rules! test_lexer {
        ($(
            $name:ident {
                $(
                    $raw:literal => $($kind:expr),*$(,)?
                );*$(;)?
            }
        )*) => {
            $(
                #[test]
                fn $name() {
                    #[allow(unused_imports)]
                    use crate::lexing::{Lexer, TokenSpan, Location, TokenKind::*};

                    let mut lexer = Lexer::new(join!(" "; $($raw)*));

                    let expected_kinds = [$(&[$($kind),*]),*];
                    let raws = [$($raw),*];

                    let mut pos = 0;

                    for (kinds, raw) in std::iter::zip(expected_kinds.into_iter(), raws.into_iter()) {
                        if kinds.len() == 1 {
                            let start = pos;
                            let end = start + raw.len();

                            let token = lexer.lex();

                            let kind: &TokenKind = &kinds[0];

                            assert_eq!(kind, token.kind());
                            assert_eq!(
                                TokenSpan::new(
                                    // we don't care about the line and the column because they are not checked in the `PartialEq` implementation of `Location`
                                    Location::new(start, 0, 0),
                                    Location::new(end, 0, 0),
                                ),
                                token.span()
                            );
                        } else {
                            for kind in kinds {
                                assert_eq!(kind, lexer.lex().kind());
                            }
                        }

                        pos += raw.len() + 1;
                    }

                    let remaining = lexer.lex_all();

                    let TokenKind::Eof = remaining[0].kind() else {
                        panic!("Remaining tokens: {:#?}.", remaining);
                    };
                }
            )*
        };
    }

    test_lexer! {
        test_keyword_fn { "fn" => KeywordFn }
        test_keyword_if { "if" => KeywordIf }
        test_keyword_unless { "unless" => KeywordUnless }
        test_keyword_while { "while" => KeywordWhile }
        test_keyword_until { "until" => KeywordUntil }
        test_keyword_else { "else" => KeywordElse }
        test_keyword_forever { "forever" => KeywordForever }
        test_keyword_repeat { "repeat" => KeywordRepeat }
        test_identifier_abc { "abc" => Identifier("abc") }
        test_identifier_abclonger { "abclonger" => Identifier("abclonger") }
        test_identifier_main { "main" => Identifier("main") }
        test_integer_0 { "0" => Int(0) }
        test_integer_65536 { "65536" => Int(65536) }
        test_integer_1 { "1" => Int(1) }
        test_integer_2048_malformed { "2048malformed" => MalformedInt }
        test_float_2_5 { "2.5" => Float(2.5) }
        test_float_0_2 { ".2" => Float(0.2) }
        test_float_2_0 { "2." => MalformedFloat }
        test_float_20_48_malformed { "20.48malformed" => MalformedFloat }
        test_string_empty { "\"\"" => String("".into()) }
        test_string_this_is_a_string_literal { "\"this is a string literal\"" => String("this is a string literal".into()) }
        test_string_escape_sequences { "\"hello\\nhi\\twhat\\ridk\\\\yes\\0or\\\"no\"" => String("hello\nhi\twhat\ridk\\yes\0or\"no".into()) }
        test_string_ascii_unicode_escape { "\"\\x5E\\x6F\\x61\\u{102}\\u{12345}\\u{103456}\"" => String("\x5E\x6F\x61\u{102}\u{12345}\u{103456}".into()) }
        test_string_malformed { "\"malformed" => MalformedString }
        test_character_a { "'a'" => Character('a') }
        test_character_b { "'b'" => Character('b') }
        test_character_ascii_escape { "'\\x5E'" => Character('\x5E') }
        test_character_unicode_escape { "'\\u{102}'" => Character('\u{102}') }
        test_character_malformed { "'m" => MalformedCharacter }
        test_left_paren { "(" => LeftParen }
        test_right_paren { ")" => RightParen }
        test_left_bracket { "[" => LeftBracket }
        test_right_bracket { "]" => RightBracket }
        test_left_brace { "{" => LeftBrace }
        test_right_brace { "}" => RightBrace }
        test_left_chevron { "<" => LeftChevron }
        test_right_chevron { ">" => RightChevron }
        test_dot { "." => Dot }
        test_comma { "," => Comma }
        test_colon { ":" => Colon }
        test_semicolon { ";" => Semicolon }
        test_plus { "+" => Plus }
        test_minus { "-" => Minus }
        test_asterisk { "*" => Asterisk }
        test_slash { "/" => Slash }
        test_ampersand { "&" => Ampersand }
        test_pipe { "|" => Pipe }
        test_equal { "=" => Equal }
        test_plus_equal { "+=" => PlusEqual }
        test_minus_equal { "-=" => MinusEqual }
        test_asterisk_equal { "*=" => AsteriskEqual }
        test_slash_equal { "/=" => SlashEqual }
        test_ampersand_equal { "&=" => AmpersandEqual }
        test_pipe_equal { "|=" => PipeEqual }
        test_equal_equal { "==" => EqualEqual }
        test_not_equal { "!=" => BangEqual }
        test_less_or_equal { "<=" => LessOrEqual }
        test_greater_or_equal { ">=" => GreaterOrEqual }
        test_left_shift { "<<" => LeftShift }
        test_right_shift { ">>" => RightShift }
        test_not { "!" => Bang }
        test_plus_plus { "++" => PlusPlus }
        test_minus_minus { "--" => MinusMinus }
        test_line_comment { "// hello how are you ?\n" => }
        test_line_comment_eof { "// hello how are you ?" => }
        test_block_comment { "/* hello how are you ? */" => }
        test_block_comment_lf { "/* hello how\nare you ? */" => }
        test_block_comment_malformed { "/* hello how\nare you ?* / *" => }
        test_eof { "" => Eof }

        test_fn_main {
            "fn" => KeywordFn;
            "main" => Identifier("main");
            "(" => LeftParen;
            ")" => RightParen;
            "{" => LeftBrace;
            "}" => RightBrace;
        }

        test_cool_program {
            r#"fn main () {
                print("Hello, world!");
            }"#
            => KeywordFn, Identifier("main"), LeftParen, RightParen, LeftBrace,
                Identifier("print"), LeftParen, String("Hello, world!".into()), RightParen, Semicolon,
            RightBrace
        }
    }
}
