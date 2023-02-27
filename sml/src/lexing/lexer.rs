use std::{
    num::{IntErrorKind, ParseIntError},
    ops::Not,
};

use crate::diagnostics::{DiagnosticsList, LexerDiagnostic};

use super::{Cursor, Token, TokenSpan};

pub struct Lexer<'a, 'b> {
    source: &'a str,
    cursor: Cursor<'a>,
    diagnostics: &'b DiagnosticsList<'a>,
}

impl<'a: 'b, 'b> Lexer<'a, 'b> {
    pub fn new(source: &'a str, diagnostics: &'b DiagnosticsList<'a>) -> Self {
        Self {
            source,
            cursor: Cursor::new(source),
            diagnostics,
        }
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
            return Token::eof(self.cursor.location().into());
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
            '(' => Token::left_paren,
            ')' => Token::right_paren,
            '[' => Token::left_bracket,
            ']' => Token::right_bracket,
            '{' => Token::left_brace,
            '}' => Token::right_brace,

            '.' => Token::dot,
            ',' => Token::comma,
            ':' => Token::colon,
            ';' => Token::semicolon,

            '=' | '!' | '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' => {
                if let Some('=') = self.cursor.peek() {
                    self.cursor.consume();
                    match c {
                        '=' => Token::equal_equal,
                        '!' => Token::bang_equal,
                        '+' => Token::plus_equal,
                        '-' => Token::minus_equal,
                        '*' => Token::asterisk_equal,
                        '/' => Token::slash_equal,
                        '&' => Token::ampersand_equal,
                        '|' => Token::pipe_equal,
                        '<' => Token::less_or_equal,
                        '>' => Token::greater_or_equal,
                        _ => unreachable!(),
                    }
                } else {
                    match c {
                        '=' => Token::equal,
                        '!' => Token::bang,
                        '+' => {
                            if let Some('+') = self.cursor.peek() {
                                self.cursor.consume();
                                Token::plus_plus
                            } else {
                                Token::plus
                            }
                        }
                        '-' => {
                            if let Some('-') = self.cursor.peek() {
                                self.cursor.consume();
                                Token::minus_minus
                            } else {
                                Token::minus
                            }
                        }
                        '*' => Token::asterisk,
                        '/' => Token::slash,
                        '&' => Token::ampersand,
                        '|' => Token::pipe,
                        '<' => {
                            if let Some('<') = self.cursor.peek() {
                                self.cursor.consume();
                                Token::left_shift
                            } else {
                                Token::left_chevron
                            }
                        }
                        '>' => {
                            if let Some('>') = self.cursor.peek() {
                                self.cursor.consume();
                                Token::right_shift
                            } else {
                                Token::right_chevron
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }

            _ => {
                let span = TokenSpan::new(start_location, self.cursor.location());
                self.diagnostics
                    .push((LexerDiagnostic::IllegalCharacter(c), span).into());
                return self.lex();
            }
        };

        let span = TokenSpan::new(start_location, self.cursor.location());

        kind(span)
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
            "fn" => Token::keyword_fn,
            "if" => Token::keyword_if,
            "unless" => Token::keyword_unless,
            "while" => Token::keyword_while,
            "until" => Token::keyword_until,
            "else" => Token::keyword_else,
            "forever" => Token::keyword_forever,
            "repeat" => Token::keyword_repeat,

            _ => return Some(Token::identifier(text, span)),
        };

        Some(kind(span))
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
            return Some(Token::float(None, span));
        }

        #[allow(clippy::collapsible_else_if)]
        Some(if !is_float {
            Token::int(
                has_word
                    .not()
                    .then(|| {
                        let res: Result<u64, ParseIntError> = text.parse();

                        if let Err(ref err) = res {
                            let IntErrorKind::PosOverflow = err.kind() else {
                                panic!("Integer parsing failed for an unhandled reason: {:?}", err);
                            };

                            self.diagnostics
                                .push((LexerDiagnostic::InvalidIntegerTooLarge, span).into());
                        }

                        res.ok()
                    })
                    .flatten(),
                span,
            )
        } else {
            Token::float(
                has_word
                    .not()
                    .then(|| {
                        let res = text.parse();

                        if res.is_err() {
                            self.diagnostics
                                .push((LexerDiagnostic::InvalidFloat, span).into());
                        }

                        res.ok()
                    })
                    .flatten(),
                span,
            )
        })
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
                        self.diagnostics.push(
                            (
                                LexerDiagnostic::UnclosedString,
                                TokenSpan::new(start_location, self.cursor.location()),
                            )
                                .into(),
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

        let span = TokenSpan::new(start_location, self.cursor.location());

        Some(Token::string(string.map(Into::into), span))
    }

    fn try_lex_character(&mut self) -> Option<Token<'a>> {
        let start_location = self.cursor.location();

        let Some('\'') = self.cursor.peek() else {
            return None;
        };
        self.cursor.consume();

        let c = self
            .cursor
            .next()
            .and_then(|c| {
                if c != '\\' {
                    Some(c)
                } else {
                    self.try_parse_escape_sequence()
                }
            })
            .and_then(|c| matches!(self.cursor.next(), Some('\'')).then_some(c));

        Some(Token::character(
            c,
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
                    .push((LexerDiagnostic::InvalidEscapeCharacter(c), span).into());
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
                self.diagnostics.push(
                    (
                        LexerDiagnostic::ExpectAsciiCharacterFirst(first),
                        TokenSpan::new(first_location, second_location),
                    )
                        .into(),
                );
                return None;
            }
            // an ascii character is 7 bits long in UTF-8, so the first byte must not exceed a value of 0x7.
            Some(n) if n > 0x7 => {
                self.diagnostics.push(
                    (
                        LexerDiagnostic::InvalidAsciiCharacterCode(first, second),
                        TokenSpan::new(first_location, last_location),
                    )
                        .into(),
                );
                return None;
            }
            Some(n) => n as u8,
        };

        let second = match second.to_digit(16) {
            None => {
                self.diagnostics.push(
                    (
                        LexerDiagnostic::ExpectAsciiCharacterSecond(second),
                        TokenSpan::new(second_location, last_location),
                    )
                        .into(),
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
            self.diagnostics.push(
                (
                    LexerDiagnostic::InvalidUnicodeSequenceMissingLeftBrace,
                    TokenSpan::new(start_index, code_index),
                )
                    .into(),
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
                            .push((LexerDiagnostic::InvalidUnicodeCharacterCode, span).into());
                        return None;
                    }
                };
            }

            if i > 5 {
                self.diagnostics.push(
                    (
                        LexerDiagnostic::InvalidUnicodeTooLong,
                        TokenSpan::new(code_index, self.cursor.location()),
                    )
                        .into(),
                );
                return None;
            }

            let digit = match next_char.to_digit(16) {
                None => {
                    let span = TokenSpan::new(self.cursor.location() - 1, self.cursor.location());
                    self.diagnostics
                        .push((LexerDiagnostic::InvalidUnicodeDigit(next_char), span).into());
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

                self.diagnostics.push(
                    (
                        LexerDiagnostic::UnclosedBlockComment,
                        TokenSpan::new(start_location, self.cursor.location()),
                    )
                        .into(),
                );

                true
            }

            None | Some(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! join {
        ($sep:literal; $first:literal $($other:literal)*) => {
            concat!($first, $($sep, $other),*)
        }
    }

    macro_rules! test_lexer {
        ($(
            $name:ident {
                $(
                    $raw:literal
                        => $($token_discr:ident $({
                            $($arg_name:ident: $arg_val:expr),*$(,)?
                        })?),*$(,)?
                );*$(;)?
            }
        )*) => {
            $(
                #[test]
                fn $name() {
                    #[allow(unused_imports)]
                    use crate::lexing::{Lexer, TokenSpan, Location, Token, TokenDiscr, token};

                    let diagnostics = Vec::new();
                    let mut lexer = Lexer::new(join!(" "; $($raw)*), &mut diagnostics);

                    let mut pos = 0;

                    $(
                        let expecteds: &[&dyn Fn(crate::lexing::TokenSpan) -> Token<'static>] = &[$(&|span| Token::$token_discr(token::$token_discr {
                            $($($arg_name: $arg_val,)*)?
                            span
                        })),*];

                        let raw = $raw;

                        if expecteds.len() == 1 {
                            let start = pos;
                            let end = start + raw.len();

                            let token = lexer.lex();

                            let expected = &expecteds[0];

                            assert_eq!(expected(
                                TokenSpan::new(
                                    // we don't care about the line and the column because they are not checked in the `PartialEq` implementation of `Location`
                                    Location::new(start, 0, 0),
                                    Location::new(end, 0, 0),
                                ),
                            ), token);
                        } else {
                            $(
                                let token = lexer.lex();

                                let Token::$token_discr(token) = token else {
                                    panic!("The token kind of the parsed token does not match: {:?} != {}", token, stringify!($token_discr));
                                };
                                $($(
                                    assert_eq!($arg_val, *token.$arg_name());
                                )*)?
                            )*
                        }

                        pos += raw.len() + 1;
                    )*

                    let remaining = lexer.lex_all();

                    let TokenDiscr::Eof = remaining[0].discr() else {
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
        test_identifier_abc { "abc" => Identifier { name: "abc" } }
        test_identifier_abclonger { "abclonger" => Identifier { name: "abclonger" } }
        test_identifier_main { "main" => Identifier { name: "main" } }
        test_integer_0 { "0" => Int { value: Some(0) } }
        test_integer_65536 { "65536" => Int { value: Some(65536) } }
        test_integer_1 { "1" => Int { value: Some(1) } }
        test_integer_2048_malformed { "2048malformed" => Int { value: None } }
        test_float_2_5 { "2.5" => Float { value: Some(2.5) } }
        test_float_0_2 { ".2" => Float { value: Some(0.2) } }
        test_float_2_0 { "2." => Float { value: None } }
        test_float_20_48_malformed { "20.48malformed" => Float { value: None } }
        test_string_empty { "\"\"" => String { value: Some("".into()) } }
        test_string_this_is_a_string_literal { "\"this is a string literal\"" => String { value: Some("this is a string literal".into()) } }
        test_string_escape_sequences { "\"hello\\nhi\\twhat\\ridk\\\\yes\\0or\\\"no\"" => String { value: Some("hello\nhi\twhat\ridk\\yes\0or\"no".into()) } }
        test_string_ascii_unicode_escape { "\"\\x5E\\x6F\\x61\\u{102}\\u{12345}\\u{103456}\"" => String { value: Some("\x5E\x6F\x61\u{102}\u{12345}\u{103456}".into()) } }
        test_string_malformed { "\"malformed" => String { value: None } }
        test_character_a { "'a'" => Character { value: Some('a') } }
        test_character_b { "'b'" => Character { value: Some('b') } }
        test_character_ascii_escape { "'\\x5E'" => Character { value: Some('\x5E') } }
        test_character_unicode_escape { "'\\u{102}'" => Character { value: Some('\u{102}') } }
        test_character_malformed { "'m" => Character { value: None } }
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
        test_eof { "" => }

        test_fn_main {
            "fn" => KeywordFn;
            "main" => Identifier{name: "main"};
            "(" => LeftParen;
            ")" => RightParen;
            "{" => LeftBrace;
            "}" => RightBrace;
        }

        test_cool_program {
            r#"fn main () {
                print("Hello, world!");
            }"#
            => KeywordFn, Identifier{name: "main"}, LeftParen, RightParen, LeftBrace,
                Identifier{name: "print"}, LeftParen, String{value: Some("Hello, world!".into())}, RightParen, Semicolon,
            RightBrace
        }
    }
}
