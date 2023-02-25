use std::ops::{Add, AddAssign, Sub};

#[derive(Debug, Copy, Clone)]
pub struct Location {
    byte: usize,
    line: usize,
    column: usize,
}

impl Location {
    pub const ZERO: Self = Self {
        byte: 0,
        line: 0,
        column: 0,
    };

    pub fn new(byte: usize, line: usize, column: usize) -> Self {
        Self { byte, line, column }
    }

    pub fn byte(&self) -> usize {
        self.byte
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn new_line(&mut self) {
        self.byte += 1;
        self.line += 1;
        self.column = 0;
    }
}

impl PartialEq for Location {
    fn eq(&self, other: &Self) -> bool {
        self.byte == other.byte
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.byte.partial_cmp(&other.byte)
    }
}

impl Add<usize> for Location {
    type Output = Self;

    fn add(self, other: usize) -> Self::Output {
        Self {
            byte: self.byte + other,
            line: self.line,
            column: self.column + other,
        }
    }
}

impl Sub<usize> for Location {
    type Output = Self;

    fn sub(self, other: usize) -> Self::Output {
        Self {
            byte: self.byte - other,
            line: self.line,
            column: self.column - other,
        }
    }
}

impl AddAssign<usize> for Location {
    fn add_assign(&mut self, other: usize) {
        self.byte += other;
        self.column += other;
    }
}
impl AddAssign<char> for Location {
    fn add_assign(&mut self, other: char) {
        if other == '\n' {
            self.new_line();
        } else {
            *self += other.len_utf8();
        }
    }
}
