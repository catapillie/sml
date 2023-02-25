mod diagnostic;
mod diagnostic_list;
mod kind;
mod severity;

pub use {
    diagnostic::Diagnostic,
    diagnostic_list::DiagnosticList,
    kind::{DiagnosticKind, LexerDiagnosticKind, ParserDiagnosticKind},
    severity::Severity,
};
