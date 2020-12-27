use crate::is_debug;
use crate::types::{Value, ValueArray};

#[derive(Debug)]
pub enum OpCode {
  Constant { location: usize },
  Add,
  Subtract,
  Multiply,
  Divide,
  Negate,
  Return,
}

#[derive(Debug)]
pub enum Error {
  Compile,
  Runtime,
}

pub type VMResult = std::result::Result<(), Error>;

pub struct VM {}

impl VM {
  pub fn new() -> Self {
    Self {}
  }

  pub fn run(&mut self, chunk: Chunk) -> VMResult {
    let mut sp: usize = 0;
    let mut stack: Vec<Value> = Vec::new();

    for (ip, instruction) in chunk.code.iter().enumerate() {
      if is_debug!() {
        for slot in &stack {
          print!("[ {} ]", slot);
        }
        if !stack.is_empty() {
          println!();
        }

        chunk.print_instruction(ip, instruction);
      }
      match instruction {
        OpCode::Constant { location } => {
          let constant = &chunk.constants[*location];
          stack.push(constant.clone());
        }
        OpCode::Add => {
          let res = self.operate_on(&mut stack, |a, b| a + b)?;
          stack.push(res);
        }
        OpCode::Subtract => {
          let res = self.operate_on(&mut stack, |a, b| a - b)?;
          stack.push(res);
        }
        OpCode::Multiply => {
          let res = self.operate_on(&mut stack, |a, b| a * b)?;
          stack.push(res);
        }
        OpCode::Divide => {
          let res = self.operate_on(&mut stack, |a, b| a / b)?;
          stack.push(res);
        }
        OpCode::Negate => {
          if let Some(v) = stack.pop() {
            stack.push(-v);
          } else {
            return Err(Error::Runtime); // tried to negate a void value
          }
        }
        OpCode::Return => {
          if let Some(v) = stack.pop() {
            println!("{}", v);
          }
          return Ok(());
        }
      }
    }
    Ok(())
  }

  fn operate_on(
    &mut self,
    stack: &mut Vec<Value>,
    f: fn(a: Value, b: Value) -> Value,
  ) -> Result<Value, Error> {
    if let Some(a) = stack.pop() {
      if let Some(b) = stack.pop() {
        Ok(f(a, b))
      } else {
        Err(Error::Runtime) // cannot add with void on rhs
      }
    } else {
      Err(Error::Runtime) // cannot add with void on lhs
    }
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

  fn print_instruction(&self, offset: usize, instruction: &OpCode) {
    print!("{:04} ", offset);
    if offset > 0 && self.line_at(offset) == self.line_at(offset - 1) {
      print!("   | ");
    } else {
      print!("{:04} ", self.line_at(offset));
    }
    match instruction {
      OpCode::Return => println!("RETURN"),
      OpCode::Negate => println!("NEGATE"),
      OpCode::Add => println!("ADD"),
      OpCode::Subtract => println!("SUBTRACT"),
      OpCode::Multiply => println!("MULTIPLY"),
      OpCode::Divide => println!("DIVIDE"),
      OpCode::Constant { location } => println!(
        "{:<16} {:04} {}",
        "CONSTANT", location, self.constants[*location]
      ),
    }
  }

  fn print_header(&self) {
    println!("== {} ==", self.name);
    println!("{:<4} {:<4} {:<16} {:<4}", "off", "line", "opcode", "extra");
  }

  fn print(&self) {
    self.print_header();
    self.code.iter().enumerate().for_each(|(offset, inst)| {
      self.print_instruction(offset, inst);
    });
  }
}
