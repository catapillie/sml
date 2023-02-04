use std::{env, fs};

use sml::Lexer;

fn main() {
    let path = env::args().nth(1).expect("not enough arguments");
    let source = fs::read_to_string(path).expect("couldn't read file");

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex_all();

    for tok in tokens {
        println!("{tok:?}")
    }
}
