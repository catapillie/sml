use crate::lexing::{token_span::TokenSpan, token_discr::TokenDiscr};

use super::Severity::{self, Error};

/// Represents a compiler diagnostic.
#[derive(Debug)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    span: TokenSpan,
}

#[derive(Debug, enum_assoc::Assoc)]
#[func(pub const fn id(&self) -> u32)]
#[func(pub const fn severity(&self) -> Severity)]
#[rustfmt::skip]
pub enum DiagnosticKind {
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

    #[assoc(id=0101)] #[assoc(severity=Error)] UnexpectedToken{expected: TokenDiscr, found: TokenDiscr},
}

// TODO: derive proc macro
// #[derive(Debug, sml_macros::Diagnostic)]
// pub enum DiagnosticKind {
//     #[diagnostic(
//         id=0001,
//         severity=Error,
//         message("Illegal character '{0}'"))]
//     IllegalCharacter(char),

//     #[diagnostic(
//         id=0002,
//         severity=Error,
//         message("The integer literal '{source}' has trailing alphabetic characters and is thus invalid"))]
//     InvalidIntegerTrailingWord,

// ...
// }

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, span: TokenSpan) -> Self {
        Self { kind, span }
    }

    pub fn build_message(&self, source: &str) -> String {
        let source = self.span.slice(source);

        format!("{:?}:[{}] {}",
            self.kind.severity(),
            self.kind.id(),

            match &self.kind {
                DiagnosticKind::IllegalCharacter(c) => format!("Illegal character '{c}'"),
                DiagnosticKind::InvalidIntegerTrailingWord => format!("The integer literal '{source}' has trailing alphabetic characters and is thus invalid"),
                DiagnosticKind::InvalidIntegerTooLarge => format!("The integer literal '{source}' is too large and cannot fit within 8 bytes"),
                DiagnosticKind::InvalidEscapeCharacter(c) => format!("The escape character '{c}' is invalid"),
                DiagnosticKind::ExpectAsciiCharacterFirst(c) => format!("Expected a valid ASCII character code, but the first character '{c}' isn't a hexadecimal digit"),
                DiagnosticKind::ExpectAsciiCharacterSecond(c) => format!("Expected a valid ASCII character code, but the second character '{c}' isn't a hexadecimal digit"),
                DiagnosticKind::InvalidAsciiCharacterCode(first, second) => format!("The ASCII character code '{first}{second}' is invalid"),
                DiagnosticKind::InvalidUnicodeSequenceMissingLeftBrace => "Unicode escape sequence must start with an opening brace".to_string(),
                DiagnosticKind::InvalidUnicodeCharacterCode => format!("The Unicode sequence '{source}' is invalid"),
                DiagnosticKind::InvalidUnicodeTooLong => "The Unicode escape sequence is too long, and is thus invalid".to_string(),
                DiagnosticKind::InvalidUnicodeDigit(c) => format!("The Unicode sequence must be a hexadecimal number, but '{c}' isn't a hexadecimal digit"),
                
                DiagnosticKind::UnexpectedToken{expected, found} => format!("Expected {expected:?} token, but found {found:?} token"),
                
                #[allow(unreachable_patterns)]
                _ => "(no message specified...)".to_string(),
            }
        )
    }
}
