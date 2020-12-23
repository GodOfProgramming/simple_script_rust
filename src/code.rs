use crate::types::{Value, ValueArray};
use std::fmt::{self, Display};

#[derive(Debug)]
pub enum OpCode {
  Constant { location: usize },
  Return,
}

#[derive(Debug)]
pub enum Error {
  Compile,
  Runtime,
}

pub type Result = std::result::Result<(), Error>;

pub struct VM {}

impl VM {
  pub fn new() -> Self {
    Self {}
  }

  pub fn run(&mut self, chunk: Chunk) -> Result {
    let sp: usize = 0;
    let stack: Vec<Value> = Vec::new();

    for (ip, instruction) in chunk.code.iter().enumerate() {
      match instruction {
        OpCode::Constant { location } => {
          let constant = &chunk.constants[*location];
          println!("{}", constant);
        }
        OpCode::Return => return Ok(()),
      }
    }
    Ok(())
  }
}

pub struct Chunk {
  name: String,
  code: Vec<OpCode>,
  lines: Vec<usize>,
  last_line: usize,
  instructions_on_line: usize,
  constants: ValueArray,
}

impl Chunk {
  pub fn new(name: String) -> Self {
    Self {
      name,
      code: Vec::new(),
      lines: Vec::new(),
      last_line: 0,
      instructions_on_line: 0,
      constants: ValueArray::new(),
    }
  }

  pub fn write(&mut self, oc: OpCode, line: usize) {
    self.code.push(oc);
    self.add_line(line);
  }

  // returns the location of the added constant
  pub fn add_constant(&mut self, value: Value) -> usize {
    self.constants.write(value)
  }

  // increments the current number of instructions on a line
  // or publishes the number and resets the count
  fn add_line(&mut self, line: usize) {
    if self.last_line == line {
      // same line number
      self.instructions_on_line += 1;
    } else {
      self.lines.push(self.instructions_on_line);
      self.last_line = line;
      self.instructions_on_line = 1; // current instruction
    }
  }

  // extracts the line at the given instruction offset
  pub fn line_at(&self, offset: usize) -> usize {
    let mut accum = 0;
    for (line, num_instns) in self.lines.iter().enumerate() {
      if accum + num_instns > offset {
        return line;
      } else {
        accum += num_instns;
      }
    }
    self.lines.len()
  }

  fn print_instruction(
    &self,
    f: &mut fmt::Formatter<'_>,
    offset: usize,
    inst: &OpCode,
  ) -> fmt::Result {
    write!(f, "{:04} ", offset)?;
    if offset > 0 && self.line_at(offset) == self.line_at(offset - 1) {
      write!(f, "   | ")?;
    } else {
      write!(f, "{:04} ", self.line_at(offset))?;
    }
    match inst {
      OpCode::Return => writeln!(f, "RETURN"),
      OpCode::Constant { location } => writeln!(
        f,
        "{:<16} {:04} {}",
        "CONSTANT", location, self.constants[*location]
      ),
    }?;
    Ok(())
  }
}

impl Display for Chunk {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "== {} ==", self.name)?;

    writeln!(
      f,
      "{:<4} {:<4} {:<16} {:<4}",
      "off", "line", "opcode", "extra"
    )?;
    for (offset, inst) in self.code.iter().enumerate() {
      self.print_instruction(f, offset, inst)?;
    }
    Ok(())
  }
}
