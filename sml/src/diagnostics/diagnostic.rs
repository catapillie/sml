use crate::lexing::TokenSpan;

use super::{DiagnosticKind, LexerDiagnostic, ParserDiagnostic, Severity};

#[derive(Debug)]
pub struct Diagnostic<'a> {
    kind: DiagnosticKind<'a>,
    span: TokenSpan,
}

impl<'a> Diagnostic<'a> {
    pub fn new(kind: DiagnosticKind<'a>, span: TokenSpan) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DiagnosticKind<'a> {
        &self.kind
    }

    pub fn span(&self) -> &TokenSpan {
        &self.span
    }

    pub fn severity(&self) -> Severity {
        match &self.kind {
            DiagnosticKind::LexerDiagnostic(diagnostic) => {
                (diagnostic.severity(), LexerDiagnostic::KIND_ID).into()
            }
            DiagnosticKind::ParserDiagnostic(diagnostic) => {
                (diagnostic.severity(), ParserDiagnostic::KIND_ID).into()
            }
        }
    }
}

impl<'a, D: Into<DiagnosticKind<'a>>> From<(D, TokenSpan)> for Diagnostic<'a> {
    fn from((kind, span): (D, TokenSpan)) -> Self {
        Self {
            kind: kind.into(),
            span,
        }
    }
}
