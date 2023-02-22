use std::borrow::Cow;

use super::TokenSpan;

make_token! {
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
        value: u64,
    };

    Float {
        value: f64,
    };

    String<'a> {
        value: Cow<'a, str>,
    };

    Character {
        value: char,
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

    Eof nospan;
}

macro_rules! make_token {
    (@struct
        $Name:ident nospan
    ) => {
        #[derive(Debug)]
        pub struct $Name;
    };
    (@struct
        $Name:ident$(<$lt:lifetime>)? nospan {
            $($field_name:ident: $FieldType:ty),*$(,)?
        }
    ) => {
        #[derive(Debug)]
        pub struct $Name$(<$lt>)? {
            $($field_name: $FieldType,)*
        }
    };
    (@struct
        $Name:ident$(<$lt:lifetime>)? $({
            $($field_name:ident: $FieldType:ty),*$(,)?
        })?
    ) => {
        #[derive(Debug)]
        pub struct $Name$(<$lt>)? {
            $(
                $($field_name: $FieldType,)*
            )?
            span: crate::lexing::TokenSpan,
        }
    };
    (@span
        $token:ident nospan
    ) => {
        None
    };
    (@span
        $token:ident
    ) => {
        Some($token.span)
    };
    ($(
        $Name:ident$(<$lt:lifetime>)? $($nospan:ident)? $({$($tt:tt)*})?
    );*$(;)?) => {
        #[derive(Debug)]
        pub enum Token<'a> {
            $(
                $Name($Name$(<$lt>)?)
            ),*
        }

        #[derive(Debug)]
        pub enum TokenDiscr {
            $(
                $Name
            ),*
        }

        impl<'a> Token<'a> {
            pub fn span(&self) -> Option<TokenSpan> {
                match self {
                    $(
                        Self::$Name(token) => make_token!(@span token $($nospan)?)
                    ),*
                }
            }

            pub fn discr(&self) -> TokenDiscr {
                match self {
                    $(
                        Self::$Name(_) => TokenDiscr::$Name
                    ),*
                }
            }
        }

        $(
            make_token! {@struct $Name$(<$lt>)? $($nospan)? $({$($tt)*})?}
        )*
    };
}

use make_token;
