mod lexer;
mod parser;
mod severity;

pub(crate) use lexer::LexerDiagnostic;
pub(crate) use parser::ParserDiagnostic;
pub use severity::Severity;

#[derive(Debug)]
pub enum Diagnostic {
    LexerDiagnostic(LexerDiagnostic),
    ParserDiagnostic(ParserDiagnostic),
}

impl Diagnostic {
    fn severity(&self) -> Severity {
        match self {
            Self::LexerDiagnostic(diagnostic) => (diagnostic.severity(), 0x10).into(),
            Self::ParserDiagnostic(diagnostic) => (diagnostic.severity(), 0x20).into(),
        }
    }
}
