use crate::{DiagnosticList, Lexer};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    diagnostics: DiagnosticList,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            diagnostics: DiagnosticList::new(),
        }
    }
}
