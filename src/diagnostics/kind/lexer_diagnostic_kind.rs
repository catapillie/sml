use super::{
    super::severity::Severity::{self, Error},
    DiagnosticKind,
};

#[derive(Debug, enum_assoc::Assoc)]
#[func(pub const fn id(&self) -> u32)]
#[func(pub const fn severity(&self) -> Severity)]
#[rustfmt::skip]
pub enum LexerDiagnosticKind {
    #[assoc(id=0001)] #[assoc(severity=Error)] IllegalCharacter(char),
    #[assoc(id=0002)] #[assoc(severity=Error)] InvalidIntegerTrailingWord,
    #[assoc(id=0003)] #[assoc(severity=Error)] InvalidIntegerTooLarge,
    #[assoc(id=0004)] #[assoc(severity=Error)] InvalidEscapeCharacter(char),
    #[assoc(id=0005)] #[assoc(severity=Error)] ExpectAsciiCharacterFirst(char),
    #[assoc(id=0006)] #[assoc(severity=Error)] ExpectAsciiCharacterSecond(char),
    #[assoc(id=0007)] #[assoc(severity=Error)] InvalidAsciiCharacterCode(char, char),
    #[assoc(id=0008)] #[assoc(severity=Error)] InvalidUnicodeSequenceMissingLeftBrace,
    #[assoc(id=0009)] #[assoc(severity=Error)] InvalidUnicodeCharacterCode,
    #[assoc(id=0010)] #[assoc(severity=Error)] InvalidUnicodeTooLong,
    #[assoc(id=0011)] #[assoc(severity=Error)] InvalidUnicodeDigit(char),
}

impl DiagnosticKind for LexerDiagnosticKind {
    fn id(&self) -> u32 {
        self.id()
    }

    fn severity(&self) -> Severity {
        self.severity()
    }

    #[rustfmt::skip]
    fn message(&self, source: &str) -> String {
        match self {
            LexerDiagnosticKind::IllegalCharacter(c) => format!("Illegal character '{c}'"),
            LexerDiagnosticKind::InvalidIntegerTrailingWord => format!("The integer literal '{source}' has trailing alphabetic characters and is thus invalid"),
            LexerDiagnosticKind::InvalidIntegerTooLarge => format!("The integer literal '{source}' is too large and cannot fit within 8 bytes"),
            LexerDiagnosticKind::InvalidEscapeCharacter(c) => format!("The escape character '{c}' is invalid"),
            LexerDiagnosticKind::ExpectAsciiCharacterFirst(c) => format!("Expected a valid ASCII character code, but the first character '{c}' isn't a hexadecimal digit"),
            LexerDiagnosticKind::ExpectAsciiCharacterSecond(c) => format!("Expected a valid ASCII character code, but the second character '{c}' isn't a hexadecimal digit"),
            LexerDiagnosticKind::InvalidAsciiCharacterCode(first, second) => format!("The ASCII character code '{first}{second}' is invalid"),
            LexerDiagnosticKind::InvalidUnicodeSequenceMissingLeftBrace => "Unicode escape sequence must start with an opening brace".to_string(),
            LexerDiagnosticKind::InvalidUnicodeCharacterCode => format!("The Unicode sequence '{source}' is invalid"),
            LexerDiagnosticKind::InvalidUnicodeTooLong => "The Unicode escape sequence is too long, and is thus invalid".to_string(),
            LexerDiagnosticKind::InvalidUnicodeDigit(c) => format!("The Unicode sequence must be a hexadecimal number, but '{c}' isn't a hexadecimal digit"),
        }
    }
}
