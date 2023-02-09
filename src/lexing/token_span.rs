use std::ops::Range;

#[derive(Debug, Copy, Clone)]
pub struct TokenSpan {
    start: usize,
    end: usize,
}

impl TokenSpan {
    pub fn new(start: usize, end: usize) -> Self {
        if start > end {
            panic!("invalid span creation");
        }
        Self { start, end }
    }

    pub fn empty(at: usize) -> Self {
        Self { start: at, end: at }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn slice<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}

impl From<Range<usize>> for TokenSpan {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
impl From<TokenSpan> for Range<usize> {
    fn from(value: TokenSpan) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
