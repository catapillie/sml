use crate::lexing::TokenDiscr;

use super::{
    DiagnosticKind,
    Severity::{self, Error},
};

#[derive(Debug, enum_assoc::Assoc)]
#[func(pub const fn id(&self) -> u32)]
#[func(pub const fn severity(&self) -> Severity)]
#[rustfmt::skip]
pub enum ParserDiagnosticKind {
    #[assoc(id=0101)] #[assoc(severity=Error)] UnexpectedToken{expected: TokenDiscr, found: TokenDiscr},
    #[assoc(id=0102)] #[assoc(severity=Error)] UnexpectedEof{expected: TokenDiscr},
    #[assoc(id=0103)] #[assoc(severity=Error)] ExpectedExpression,
    #[assoc(id=0104)] #[assoc(severity=Error)] ExpectedStatement,
}

impl DiagnosticKind for ParserDiagnosticKind {
    fn id(&self) -> u32 {
        self.id()
    }

    fn severity(&self) -> Severity {
        self.severity()
    }

    #[rustfmt::skip]
    fn message(&self, source: &str) -> String {
        use ParserDiagnosticKind::*;
        
        match self {
            UnexpectedToken { expected, found } => format!("Expected {expected:?} token, but found {found:?} token"),
            UnexpectedEof { expected } => format!("Expected {expected:?} token, but unexpectedly reached end-of-file"),
            ExpectedExpression => "Expected an expression".to_string(),
            ExpectedStatement => "Expected a statement".to_string(),
        }
    }
}
