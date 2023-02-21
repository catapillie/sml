use crate::lexing::token_span::TokenSpan;

use super::kind::DiagnosticKind;

/// Represents a compiler diagnostic.
#[derive(Debug)]
pub struct Diagnostic<T: DiagnosticKind> {
    kind: T,
    span: TokenSpan,
}

impl<T: DiagnosticKind> Diagnostic<T> {
    pub fn new(kind: T, span: TokenSpan) -> Self {
        Self { kind, span }
    }

    pub fn build_message(&self, source: &str) -> String {
        let source = self.span.slice(source);

        format!(
            "{:?}:[{:04}] {}",
            self.kind.severity(),
            self.kind.id(),
            self.kind.message(source),
        )
    }
}
