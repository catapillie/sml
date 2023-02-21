use super::Location;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct TokenSpan {
    start: Location,
    end: Location,
}

impl TokenSpan {
    pub fn new(start: Location, end: Location) -> Self {
        if start > end {
            panic!("invalid span creation");
        }
        Self { start, end }
    }

    pub fn empty(at: Location) -> Self {
        Self { start: at, end: at }
    }

    pub fn start(&self) -> Location {
        self.start
    }

    pub fn end(&self) -> Location {
        self.end
    }

    pub fn slice<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start.byte()..self.end.byte()]
    }
}
