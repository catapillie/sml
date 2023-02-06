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
            "[{:?}] {} ({}..{})",
            self.severity,
            self.message,
            self.span.start(),
            self.span.end()
        )
    }
}

impl Diagnostic {
    // helper functions for diagnostic creation
    pub fn illegal_character(c: char, span: TokenSpan) -> Self {
        Self {
            severity: Severity::Error,
            message: format!("Illegal character '{c}'"),
            span,
        }
    }
}
