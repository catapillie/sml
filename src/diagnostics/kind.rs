mod lexer_diagnostic_kind;
mod parser_diagnostic_kind;

pub use {
    lexer_diagnostic_kind::LexerDiagnosticKind,
    parser_diagnostic_kind::ParserDiagnosticKind,
};

use super::Severity;

pub trait DiagnosticKind {
    fn id(&self) -> u32;
    fn severity(&self) -> Severity;
    fn message(&self, source: &str) -> String;
}