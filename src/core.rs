use crate::ast;
use crate::env::{Env, EnvRef};
use crate::lex;
use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;

pub struct Interpreter {
  globals: EnvRef,
}

impl Interpreter {
  pub fn new() -> Interpreter {
    Interpreter {
      globals: Rc::new(RefCell::new(Env::new())),
    }
  }

  pub fn run_interactive(&self) -> io::Result<()> {
    let mut input = String::new();
    let exit = false;
    let mut line_number = 0;

    while !exit {
      print!("ss(main):{}> ", line_number);
      io::stdout().flush()?;
      io::stdin().read_line(&mut input)?;
      match self.run(&input) {
        Ok(lines_executed) => line_number += lines_executed,
        Err((msg, err_line)) => println!("{}: {}", msg, line_number + err_line),
      }
      input.clear();
    }

    Ok(())
  }

  pub fn run(&self, src: &str) -> Result<usize, (String, usize)> {
    let (lines_executed, tokens) = match lex::analyze(src) {
      Ok(tuple) => tuple,
      Err(line) => {
        return Err((String::from("analyze error"), line));
      }
    };

    let prgm = match ast::parse(&tokens) {
      Ok(ast) => ast,
      Err(err) => return Err((format!("parse error: {}", err.msg), err.line)),
    };

    let value = match ast::exec(Rc::clone(&self.globals), prgm) {
      Ok(v) => v,
      Err(err) => return Err((err.msg, err.line)),
    };

    println!("=> {}", value);

    Ok(lines_executed)
  }
}
