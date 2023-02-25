use super::severity::LowSeverity;

#[derive(Debug)]
pub enum LexerDiagnostic {}

impl LexerDiagnostic {
    pub fn severity(&self) -> LowSeverity {
        LowSeverity::Error(self.id())
    }

    fn id(&self) -> u16 {
        0x0
    }
}
