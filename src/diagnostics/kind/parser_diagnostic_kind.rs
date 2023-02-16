use crate::lexing::token_discr::TokenDiscr;

use super::{
    super::severity::Severity::{self, Error},
    DiagnosticKind,
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
        match self {
            ParserDiagnosticKind::UnexpectedToken { expected, found } => format!("Expected {expected:?} token, but found {found:?} token"),
            ParserDiagnosticKind::UnexpectedEof { expected } => format!("Expected {expected:?} token, but unexpectedly reached end-of-file"),
            ParserDiagnosticKind::ExpectedExpression => "Expected an expression".to_string(),
            ParserDiagnosticKind::ExpectedStatement => "Expected a statement".to_string(),
        }
    }
}
