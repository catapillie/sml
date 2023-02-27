use std::cell::RefCell;

use super::Diagnostic;

pub struct DiagnosticsList<'a>(RefCell<Vec<Diagnostic<'a>>>);

impl<'a> DiagnosticsList<'a> {
    pub fn new() -> Self {
        Self(RefCell::new(Vec::new()))
    }

    pub fn push(&self, diagnostic: Diagnostic<'a>) {
        self.0.borrow_mut().push(diagnostic);
    }

    pub fn to_vec(self) -> Vec<Diagnostic<'a>> {
        self.0.into_inner()
    }
}

impl<'a> Default for DiagnosticsList<'a> {
    fn default() -> Self {
        Self::new()
    }
}
