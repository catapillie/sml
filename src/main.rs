use std::{env, fs};

use sml::{Lexer, lexing::token_kind::TokenKind};

fn main() {
    let path = env::args().nth(1).expect("not enough arguments");
    let source = fs::read_to_string(path).expect("couldn't read file");

    let mut lexer = Lexer::new(&source);
    loop {
        let tok = lexer.lex();
        println!("{tok:?}");

        if *tok.kind() == TokenKind::Eof {
            break;
        }
    }
}
