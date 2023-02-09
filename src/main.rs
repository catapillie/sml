use std::{env, fs};

use sml::Parser;

fn main() {
    let path = env::args().nth(1).expect("not enough arguments");
    let source = fs::read_to_string(path).expect("couldn't read file");

    let mut parser = Parser::new(&source);

    let expression = parser.parse_expression();

    let lexer_diagnostics = parser.lexer().diagnostics();
    let parser_diagnostics = parser.diagnostics();

    if lexer_diagnostics.list().is_empty() && parser_diagnostics.list().is_empty() {
        println!("\n[OK] compilation finished successfully.\n\n{expression:?}");
        return;
    }

    // TODO: sort messages by span.
    // TODO: pretty error messages.
    println!("\n[!] compilation finished abnormally:");
    
    if !lexer_diagnostics.list().is_empty() {
        for e in lexer_diagnostics.list() {
            println!("    {}", e.build_message(&source));
        }
    }

    if !parser_diagnostics.list().is_empty() {
        for e in parser_diagnostics.list() {
            println!("    {}", e.build_message(&source));
        }
    }
}
