use std::io::{self, Write};

use sml::{DiagnosticsList, Parser};

fn main() {
    loop {
        let mut source = String::new();

        print!("\n» ");
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut source)
            .expect("couldn't read line!");

        let diagnostics = DiagnosticsList::new();
        let mut parser = Parser::new(&source, &diagnostics);

        let ast = parser.parse_statement(false);
        println!("\n{ast:#?}");

        let diagnostics = diagnostics.to_vec();

        if diagnostics.is_empty() {
            println!("\n[OK] parsing finished successfully.");
            continue;
        }

        println!("\n[!] parsing finished abnormally:");

        for d in diagnostics {
            println!("    {:?}", d); // TODO: build message
        }
    }
}
