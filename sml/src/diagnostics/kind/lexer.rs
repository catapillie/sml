use super::super::severity::KindSeverity;

#[derive(Debug, enum_assoc::Assoc)]
#[func(const fn id(&self) -> u16)]
#[rustfmt::skip]
pub enum LexerDiagnostic {
    #[assoc(id=0x0001)] IllegalCharacter(char),
    #[assoc(id=0x0002)] InvalidIntegerTrailingWord,
    #[assoc(id=0x0003)] InvalidIntegerTooLarge,
    #[assoc(id=0x0004)] InvalidFloatTrailingWord,
    #[assoc(id=0x0005)] InvalidFloat,
    #[assoc(id=0x0006)] UnclosedString,
    #[assoc(id=0x0007)] InvalidEscapeCharacter(char),
    #[assoc(id=0x0008)] ExpectAsciiCharacterFirst(char),
    #[assoc(id=0x0009)] ExpectAsciiCharacterSecond(char),
    #[assoc(id=0x000A)] InvalidAsciiCharacterCode(char, char),
    #[assoc(id=0x000B)] InvalidUnicodeSequenceMissingLeftBrace,
    #[assoc(id=0x000C)] InvalidUnicodeCharacterCode,
    #[assoc(id=0x000D)] InvalidUnicodeTooLong,
    #[assoc(id=0x000E)] InvalidUnicodeDigit(char),
    #[assoc(id=0x000F)] UnclosedBlockComment,
}

impl LexerDiagnostic {
    pub const KIND_ID: u8 = 0x10;

    pub fn severity(&self) -> KindSeverity {
        KindSeverity::Error(self.id())
    }
}
