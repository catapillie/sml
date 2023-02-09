use std::{env, fs};

use sml::{Lexer, Parser};

fn main() {
    let path = env::args().nth(1).expect("not enough arguments");
    let source = fs::read_to_string(path).expect("couldn't read file");

    let lexer = Lexer::new(&source);
    let parser = Parser::new(lexer);

    

    // let diagnostics = lexer.diagnostics();
    // if !diagnostics.list().is_empty() {
    //     println!("\n[!] compilation finished abnormally:");
    //     for e in diagnostics.list() {
    //         println!("    {e:?}");
    //     }
    // }
}
