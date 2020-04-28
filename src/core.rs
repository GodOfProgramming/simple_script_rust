use std::io::{self, Read, Write};

pub struct Interpreter {
  line_number: u64,
  depth: u64,
  is_active: bool,
}

impl Interpreter {
  pub fn new() -> Interpreter {
    Interpreter {
      line_number: 1,
      depth: 0,
      is_active: true,
    }
  }

  pub fn read_from_stdin(&mut self) -> io::Result<()> {
    print!("simple:{}:{} >> ", self.line_number, self.depth);
    io::stdout().flush()?;
    self.line_number += 1;

    let mut line = String::new();

    io::stdin().read_line(&mut line)?;

    self.run_string(line);

    Ok(())
  }

  pub fn run_string(&self, line: String) {}

  pub fn is_active(&self) -> bool {
    self.is_active
  }
}
