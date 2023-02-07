mod diagnostic;
mod diagnostic_list;
mod severity;

pub use {
    diagnostic::{Diagnostic, DiagnosticKind},
    diagnostic_list::DiagnosticList,
    severity::Severity,
};
