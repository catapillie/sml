use crate::lexing::token_span::TokenSpan;

use super::severity::Severity;

/// Represents a compiler diagnostic.
pub struct Diagnostic<'a> {
    severity: Severity,
    message: &'a str,
    span: TokenSpan,
}
