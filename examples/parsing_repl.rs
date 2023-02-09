use std::io::{self, Write};

use sml::Parser;

fn main() {
    loop {
        let mut source = String::new();

        print!("\n» ");
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut source)
            .expect("couldn't read line!");

        let mut parser = Parser::new(&source);

        let ast = parser.parse_expression();
        println!("\n{ast:#?}");

        let lexer_diagnostics = parser.lexer().diagnostics();
        let parser_diagnostics = parser.diagnostics();

        if lexer_diagnostics.list().is_empty() && parser_diagnostics.list().is_empty() {
            println!("\n[OK] parsing finished successfully.");
        } else {
            println!("\n[!] parsing finished abnormally:");

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
    }
}
