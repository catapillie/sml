use super::{Diagnostic, kind::DiagnosticKind};

use crate::lexing::token_span::TokenSpan;

pub struct DiagnosticList<T: DiagnosticKind> {
    list: Vec<Diagnostic<T>>,
}

impl<T: DiagnosticKind> DiagnosticList<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { list: Vec::new() }
    }

    pub fn push(&mut self, diagnostic: Diagnostic<T>) {
        self.list.push(diagnostic);
    }

    pub fn push_kind(&mut self, kind: T, span: TokenSpan) {
        self.list.push(Diagnostic::new(kind, span));
    }

    pub fn list(&self) -> &Vec<Diagnostic<T>> {
        &self.list
    }
}
