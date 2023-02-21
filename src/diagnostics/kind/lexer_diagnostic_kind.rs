use super::{
    Severity::{self, Error},
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
    #[assoc(id=0002)] #[assoc(severity=Error)] InvalidFloatTrailingWord,
    #[assoc(id=0003)] #[assoc(severity=Error)] InvalidFloat,
    #[assoc(id=0012)] #[assoc(severity=Error)] UnclosedString,
    #[assoc(id=0004)] #[assoc(severity=Error)] InvalidEscapeCharacter(char),
    #[assoc(id=0005)] #[assoc(severity=Error)] ExpectAsciiCharacterFirst(char),
    #[assoc(id=0006)] #[assoc(severity=Error)] ExpectAsciiCharacterSecond(char),
    #[assoc(id=0007)] #[assoc(severity=Error)] InvalidAsciiCharacterCode(char, char),
    #[assoc(id=0008)] #[assoc(severity=Error)] InvalidUnicodeSequenceMissingLeftBrace,
    #[assoc(id=0009)] #[assoc(severity=Error)] InvalidUnicodeCharacterCode,
    #[assoc(id=0010)] #[assoc(severity=Error)] InvalidUnicodeTooLong,
    #[assoc(id=0011)] #[assoc(severity=Error)] InvalidUnicodeDigit(char),
    #[assoc(id=0012)] #[assoc(severity=Error)] UnclosedBlockComment,
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
        use LexerDiagnosticKind::*;
        
        match self {
            IllegalCharacter(c) => format!("Illegal character '{c}'"),
            InvalidIntegerTrailingWord => format!("The integer literal '{source}' has trailing alphabetic characters and is thus invalid"),
            InvalidIntegerTooLarge => format!("The integer literal '{source}' is too large and cannot fit within 8 bytes"),
            InvalidFloatTrailingWord => format!("The floating point number literal '{source}' has trailing alphabetic characters and is thus invalid"),
            InvalidFloat => format!("Failed to parse the floating point number literal '{source}'"),
            UnclosedString => "Unclosed string, missing a '\"' at the end.".to_string(),
            InvalidEscapeCharacter(c) => format!("The escape character '{c}' is invalid"),
            ExpectAsciiCharacterFirst(c) => format!("Expected a valid ASCII character code, but the first character '{c}' isn't a hexadecimal digit"),
            ExpectAsciiCharacterSecond(c) => format!("Expected a valid ASCII character code, but the second character '{c}' isn't a hexadecimal digit"),
            InvalidAsciiCharacterCode(first, second) => format!("The ASCII character code '{first}{second}' is invalid"),
            InvalidUnicodeSequenceMissingLeftBrace => "Unicode escape sequence must start with an opening brace".to_string(),
            InvalidUnicodeCharacterCode => format!("The Unicode sequence '{source}' is invalid"),
            InvalidUnicodeTooLong => "The Unicode escape sequence is too long, and is thus invalid".to_string(),
            InvalidUnicodeDigit(c) => format!("The Unicode sequence must be a hexadecimal number, but '{c}' isn't a hexadecimal digit"),
            UnclosedBlockComment => "Unclosed block comment, missing a \"*/\" at the end.".to_string(),
        }
    }
}
