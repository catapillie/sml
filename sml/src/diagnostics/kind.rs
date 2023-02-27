pub mod lexer;
pub mod parser;

use {lexer::LexerDiagnostic, parser::ParserDiagnostic};

#[derive(Debug)]
pub enum DiagnosticKind<'a> {
    LexerDiagnostic(LexerDiagnostic),
    ParserDiagnostic(ParserDiagnostic<'a>),
}

impl<'a> From<LexerDiagnostic> for DiagnosticKind<'a> {
    fn from(value: LexerDiagnostic) -> Self {
        Self::LexerDiagnostic(value)
    }
}

impl<'a> From<ParserDiagnostic<'a>> for DiagnosticKind<'a> {
    fn from(value: ParserDiagnostic<'a>) -> Self {
        Self::ParserDiagnostic(value)
    }
}
