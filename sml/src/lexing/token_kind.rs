use super::Token;

pub trait TokenKind<'a>: TryFrom<Token<'a>, Error = Token<'a>> + Into<Token<'a>> + Sized {
    const NAME: &'static str;
}
