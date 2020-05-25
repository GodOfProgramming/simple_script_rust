use std::io::{self, Write};
use crate::lex::Lexer;
use crate::ast::{Parser, Printer};

pub struct Interpreter {}

impl Interpreter {
  pub fn new() -> Interpreter {
    Interpreter {}
  }

  pub fn run_interactive(&self) -> io::Result<()> {
    let mut input = String::new();
    let exit = false;
    let mut line_number = 0;

    while !exit {
      print!("ss(main):{}> ", line_number);
      io::stdout().flush()?;
      io::stdin().read_line(&mut input)?;
      match self.exec(&input) {
        Ok(lines_executed) => line_number += lines_executed,
        Err(err_line) => println!("Error found on line number {}", line_number + err_line),
      }
      input.clear();
    }

    Ok(())
  }

  pub fn exec(&self, src: &str) -> Result<usize, String> {
    let lexer = Lexer::new();

    let (lines_executed, tokens) = match lexer.analyze(src) {
        Ok(tuple) => tuple,
        Err(line) => return Err(format!("")),
    };

    let parser = Parser::new(tokens);

    let expr = match parser.parse() {
        Ok(e) => e,
        Err(s) => return Err(s),
    };

    let printer = Printer::new();
    
    println!("{}", printer.print(expr));

    Ok(lines_executed)
  }
}
