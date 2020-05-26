use crate::ast;
use crate::lex;
use std::io::{self, Write};

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
        Err((err_line, msg)) => println!("{}: {}", msg, line_number + err_line),
      }
      input.clear();
    }

    Ok(())
  }

  pub fn exec(&self, src: &str) -> Result<usize, (usize, String)> {
    let (lines_executed, tokens) = match lex::analyze(src) {
      Ok(tuple) => tuple,
      Err(line) => {
        return Err((line, String::from("analyze error")));
      }
    };

    let expr = match ast::parse(&tokens) {
      Ok(ast) => ast,
      Err(msg) => return Err((0, format!("parse error: {}", msg))),
    };

    let value = match ast::eval(expr) {
      Ok(v) => v,
      Err(msg) => return Err((0, msg)),
    };

    println!("{}", value);

    Ok(lines_executed)
  }
}
