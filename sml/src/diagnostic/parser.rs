use super::severity::LowSeverity;

#[derive(Debug)]
pub enum ParserDiagnostic {}

impl ParserDiagnostic {
    pub fn severity(&self) -> LowSeverity {
        LowSeverity::Error(self.id())
    }

    fn id(&self) -> u16 {
        0x0
    }
}
