use crate::lexing::Token;

use super::super::severity::KindSeverity;

#[derive(Debug, enum_assoc::Assoc)]
#[func(const fn id(&self) -> u16)]
#[rustfmt::skip]
pub enum ParserDiagnostic<'a> {
    #[assoc(id=0x0001)] UnexpectedToken{expected: &'static str, found: Token<'a>},
    #[assoc(id=0x0002)] UnexpectedEof{expected: &'static str},
    #[assoc(id=0x0003)] ExpectedExpression{found: Token<'a>},
    #[assoc(id=0x0004)] ExpectedStatement{found: Token<'a>},
}

impl<'a> ParserDiagnostic<'a> {
    pub const KIND_ID: u8 = 0x20;

    pub fn severity(&self) -> KindSeverity {
        KindSeverity::Error(self.id())
    }
}
