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
      match io::stdin().read_line(&mut input) {
        Ok(_) => match self.exec(&input) {
          Ok(lines) => line_number += lines,
          Err(err_line) => println!("Error found on line number {}", err_line),
        },
        Err(err) => return Err(err),
      }
    }

    Ok(())
  }

  pub fn exec(&self, _src: &str) -> Result<usize, usize> {
    // create scanner
    // scan tokens into list
    // print tokens

    let lines_executed = 0;

    Ok(lines_executed)
  }
}
