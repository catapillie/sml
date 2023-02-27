mod diagnostic;
mod kind;
mod list;
mod severity;

pub use {
    diagnostic::Diagnostic,
    kind::{lexer::LexerDiagnostic, parser::ParserDiagnostic, DiagnosticKind},
    list::DiagnosticsList,
    severity::{KindSeverity, Severity},
};
