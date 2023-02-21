use std::str::Chars;

use super::Location;

/// A struct that keeps track of the position of the cursor in the sourcce buffer.
///
/// Use [`next`](Cursor::next) to get the next character in the source, and [`peek`](Cursor::peek) to peek.
#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    /// The next character that will be read. It is [`None`] if EOF has been reached.
    next: Option<char>,
    /// An iterator over the characters of the buffer.
    chars: Chars<'a>,
    /// The location of the next character.
    location: Location,
}

impl<'a> Cursor<'a> {
    /// Creates a new [`Cursor`] bound to `source`.
    pub fn new(source: &'a str) -> Self {
        let mut chars = source.chars();

        Self {
            next: chars.next(),
            chars,
            location: Location::ZERO,
        }
    }

    /// Consumes the next character without returning it.
    ///
    /// This is just a wrapper around [`next`](Cursor::next).
    /// Its only purpose is to make it obvious that you just want to consume a character without reading it,
    /// in most cases because [`peek`](Cursor::peek) was used right before.
    pub fn consume(&mut self) {
        self.next();
    }

    /// Peeks at the next character without consuming it.
    ///
    /// The function returns [`None`] if EOF was reached.
    pub fn peek(&self) -> Option<char> {
        self.next
    }

    /// Get the offset of the next character.
    ///
    /// If EOF was reached, the offset length of the source is returned.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Creates a [`CursorPeekIter`].
    pub fn peek_iter<'b>(&'b mut self) -> CursorPeekIter<'a, 'b> {
        CursorPeekIter::new(self)
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(next) = self.next else {
            return None
        };

        self.location += next;

        self.next = self.chars.next();

        Some(next)
    }
}

/// A struct that peeks each character.
///
/// The peeked character is consumed only when [`next`](CursorPeekIter::next) is called again.
pub struct CursorPeekIter<'a, 'b> {
    /// The [`Cursor`] it is bound to.
    cursor: &'b mut Cursor<'a>,
    /// Whether the next character is the first after this struct's creation.
    ///
    /// This is useful because we don't want to consume the first character,
    /// which is what happens on other characters when [`next`](CursorPeekIter::next) is called.
    is_first: bool,
}

impl<'a, 'b> CursorPeekIter<'a, 'b> {
    /// Creates a new [`CursorPeekIter`] bound to `cursor`.
    fn new(cursor: &'b mut Cursor<'a>) -> Self {
        Self {
            cursor,
            is_first: true,
        }
    }
}

impl<'a, 'b> Iterator for CursorPeekIter<'a, 'b> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        // If this is the first peeked character, don't consume it.
        if self.is_first {
            self.is_first = false;
            self.cursor.peek()
        } else {
            // Consume the character that was peeked on the previous [`next`](`CursorPeekIter::new`) call.
            self.cursor.consume();
            self.cursor.peek()
        }
    }
}
