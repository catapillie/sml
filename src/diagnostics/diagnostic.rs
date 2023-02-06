use std::fmt::Display;

use crate::lexing::token_span::TokenSpan;

use super::severity::Severity;

/// Represents a compiler diagnostic.
pub struct Diagnostic {
    severity: Severity,
    message: String,
    span: TokenSpan,
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}..{}] {:?} {}",
            self.span.start(),
            self.span.end(),
            self.severity,
            self.message
        )
    }
}

// helper functions for diagnostic creation
impl Diagnostic {
    pub fn illegal_character(c: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("Illegal character '{c}'"),
            span,
        }
    }

    pub fn invalid_integer_trailing_word(text: &str, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("The integer literal '{text}' has trailing alphabetic characters and is thus invalid"),
            span,
        }
    }

    pub fn invalid_integer_too_large(text: &str, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("The integer literal '{text}' is too large and cannot fit within 8 bytes"),
            span,
        }
    }

    pub fn invalid_escape_character(c: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("The escape character '{c}' is invalid"),
            span,
        }
    }

    pub fn expect_ascii_character_first(c: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("Expected a valid ASCII character code, but the first character '{c}' isn't a hexadecimal digit"),
            span,
        }
    }

    pub fn expect_ascii_character_second(c: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("Expected a valid ASCII character code, but the second character '{c}' isn't a hexadecimal digit"),
            span,
        }
    }

    pub fn invalid_ascii_character_code(first: char, second: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("The ASCII character code '{first}{second}' is invalid"),
            span,
        }
    }
    
    pub fn unicode_sequence_missing_left_brace(span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: String::from("Unicode escape sequence must start with an opening brace"),
            span,
        }
    }

    pub fn invalid_unicode_character_code(text: &str, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("The Unicode sequence '{text}' is invalid"),
            span,
        }
    }

    pub fn invalid_unicode_too_long(span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: String::from("The Unicode escape sequence is too long, and is thus invalid"),
            span,
        }
    }

    pub fn invalid_unicode_digit(c: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("The Unicode sequence must be a hexadecimal number, but '{c}' isn't a hexadecimal digit"),
            span,
        }
    }
}
