#[derive(Debug)]
pub struct TokenSpan {
    start: usize,
    end: usize,
}

impl TokenSpan {
    pub const MAX: Self = Self {
        start: usize::MAX,
        end: usize::MAX,
    };

    pub fn new(start: usize, end: usize) -> Self {
        if start > end {
            panic!("invalid span creation");
        }
        Self { start, end }
    }

    pub fn empty(at: usize) -> Self {
        Self { start: at, end: at }
    }
}
