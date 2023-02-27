use std::{env, fs};

use sml::{DiagnosticsList, Parser};

fn main() {
    let path = env::args().nth(1).expect("not enough arguments");
    let source = fs::read_to_string(path).expect("couldn't read file");

    let diagnostics = DiagnosticsList::new();
    let mut parser = Parser::new(&source, &diagnostics);

    let ast = parser.parse_expression(false);

    println!("\n{ast:?}");

    let diagnostics = diagnostics.to_vec();

    if diagnostics.is_empty() {
        println!("\n[OK] compilation finished successfully.");
        return;
    }

    // TODO: sort messages by span.
    // TODO: pretty error messages.
    println!("\n[!] compilation finished abnormally:");

    for d in diagnostics {
        println!("    {:?}", d); // TODO: build message
    }
}
