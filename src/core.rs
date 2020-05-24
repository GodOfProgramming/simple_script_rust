use std::io::{self, Write};
use crate::lex::Lexer;

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

  pub fn exec(&self, src: &str) -> Result<usize, usize> {
    let lexer = Lexer::new();

    let (lines_executed, tokens) = lexer.analyze(src)?;

    for token in tokens.iter() {
      println!("{}", token);
    }

    Ok(lines_executed)
  }
}
