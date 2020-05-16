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
        Ok(_) => {
          line_number += 1;
        }
        Err(err) => return Err(err),
      }
    }

    Ok(())
  }

  pub fn run_src(&self, _src: &str) {}
}
