use std::borrow::Cow;

use sml_macros::gen_tokens;

gen_tokens! {
    KeywordFn;
    KeywordIf;
    KeywordUnless;
    KeywordWhile;
    KeywordUntil;
    KeywordElse;
    KeywordForever;
    KeywordRepeat;

    Identifier<'a> {
        name: &'a str,
    };

    Int {
        value: Option<u64>,
    };

    Float {
        value: Option<f64>,
    };

    String<'a> {
        value: Option<Cow<'a, str>>,
    };

    Character {
        value: Option<char>,
    };

    LeftParen;
    RightParen;
    LeftBracket;
    RightBracket;
    LeftBrace;
    RightBrace;
    LeftChevron;
    RightChevron;

    Dot;
    Comma;
    Colon;
    Semicolon;

    Plus;
    Minus;
    Asterisk;
    Slash;
    Ampersand;
    Pipe;

    EqualEqual;
    BangEqual;
    LessOrEqual;
    GreaterOrEqual;

    Equal;
    PlusEqual;
    MinusEqual;
    AsteriskEqual;
    SlashEqual;
    AmpersandEqual;
    PipeEqual;

    RightShift;
    LeftShift;

    Bang;
    PlusPlus;
    MinusMinus;

    Eof;
}
