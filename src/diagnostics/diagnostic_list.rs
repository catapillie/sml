use super::{Diagnostic, DiagnosticKind};

use crate::lexing::token_span::TokenSpan;

pub struct DiagnosticList {
    list: Vec<Diagnostic>,
}

impl DiagnosticList {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { list: Vec::new() }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.list.push(diagnostic);
    }

    pub fn push_kind(&mut self, kind: DiagnosticKind, span: TokenSpan) {
        self.list.push(Diagnostic::new(kind, span));
    }

    pub fn list(&self) -> &Vec<Diagnostic> {
        &self.list
    }
}
