use crate::types::{Env, Value};
use std::fmt::{self, Debug, Display};

#[derive(Debug, Clone)]
pub enum OpCode {
  /**
   * No operation instruction
   */
  NoOp,
  /**
   *  Looks up a constant value at the specified location. Location is specified by the tuple
   */
  Const(usize),
  /**
   * Pushes a nil value on to the stack
   */
  Nil,
  /**
   * Pushes a true value on to the stack
   */
  True,
  /**
   * Pushes a false value on to the stack
   */
  False,
  /**
   * Pops a value off the stack
   */
  Pop,
  /**
   * Pops N values off the stack. N is specified by tuple
   */
  PopN(usize),
  /**
   * Looks up a local variable. The index in the stack is specified by the modifying bits
   */
  LookupLocal(usize),
  /**
   * Assigns a value to the local variable indexed by the tuple. The value comes off the top of the stack
   */
  AssignLocal(usize),
  /**
   * Looks up a global variable. The name is stored in the enum
   */
  LookupGlobal(String),
  /**
   * Defines a new global variable. The name is stored in the enum. The value comes off the top of the stack
   */
  DefineGlobal(String),
  /**
   * Assigns a value to the global variable. The Name is stored in the enum. The value comes off the top of the stack
   */
  AssignGlobal(String),
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Equal,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  NotEqual,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Greater,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  GreaterEqual,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Less,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  LessEqual,
  /**
   * Pops a value off the stack, and compars it with the peeked value, pushing the new value on
   */
  Check,
  /**
   * Pops two values off the stack, calculates the sum, then pushes the result back on
   */
  Add,
  /**
   * Pops two values off the stack, calculates the difference, then pushes the result back on
   */
  Sub,
  /**
   * Pops two values off the stack, calculates the product, then pushes the result back on
   */
  Mul,
  /**
   * Pops two values off the stack, calculates the quotient, then pushes the result back on
   */
  Div,
  /**
   * Pops two values off the stack, calculates the remainder, then pushes the result back on
   */
  Mod,
  /**
   * Pops a value off the stack, inverts its truthy value, then pushes that back on
   */
  Not,
  /**
   * Pops a value off the stack, inverts its numerical value, then pushes that back on
   */
  Negate,
  /**
   * Pops a value off the stack and prints it to the screen
   */
  Print,
  /**
   * Swaps the top two values on the stack.
   */
  Swap,
  /**
   * Jumps to a code location indicated by the tuple
   */
  Jump(usize),
  /**
   * Jumps to a code location indicated by the tuple
   */
  JumpIfFalse(usize),
  /**
   * Jumps the instruction pointer backwards N instructions. N specified by the tuple
   */
  Loop(usize),
  /**
   * Peeks at the stack, if the top value is true short circuits to the instruction pointed to by the tuple
   */
  Or(usize),
  /**
   * Peeks at the stack, if the top value is false short circuits to the instruction pointed to by the tuple
   */
  And(usize),
  /**
   * Pushes the stack pointer onto the stack
   */
  PushSp,
  /** Calls the instruction on the stack. Number of arguments is specified by the modifying bits */
  Call,
  /** Exits from a function */
  Return,
}

type Instructions = Vec<OpCode>;

#[derive(Debug)]
pub enum Token {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Semicolon,
  Plus,
  Minus,
  Asterisk,
  Slash,
  Modulus,

  // One or two character tokens.
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Arrow,

  // Literals.
  Identifier(String),
  String(String),
  Number(f64),

  // Keywords.
  And,
  Break,
  Class,
  Cont,
  Else,
  End,
  False,
  For,
  Fn,
  If,
  Let,
  Load,
  LoadRel,
  Loop,
  Match,
  Nil,
  Or,
  Print,
  Return,
  True,
  While,
  Eof,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Token::Identifier(i) => write!(f, "Identifier ({})", i),
      Token::String(s) => write!(f, "String ({})", s),
      Token::Number(n) => write!(f, "Number ({})", n),
      _ => write!(f, "{:?}", self),
    }
  }
}

pub struct CodeMeta;

impl CodeMeta {
  fn line_at(&self, offset: usize) -> usize {
    todo!();
  }
}

pub struct Context {
  instructions: Instructions,
  ip: usize,

  env: Env,
  stack: Vec<Value>,
  consts: Vec<Value>,

  meta: CodeMeta,
}

impl Context {
  pub fn new(instructions: Instructions, meta: CodeMeta) -> Self {
    Self {
      instructions,
      ip: 0,
      env: Env::new(),
      stack: Vec::new(),
      consts: Vec::new(),
      meta,
    }
  }

  pub fn done(&self) -> bool {
    self.ip >= self.instructions.len()
  }

  pub fn next(&self) -> OpCode {
    self.instructions[self.ip].clone()
  }

  pub fn advance(&mut self) {
    self.ip += 1;
  }

  pub fn stack_push(&mut self, value: Value) {
    self.stack.push(value);
  }

  pub fn stack_pop(&mut self) -> Option<Value> {
    self.stack.pop()
  }

  pub fn stack_pop_n(&mut self, count: usize) {
    self.stack.truncate(self.stack.len().saturating_sub(count));
  }

  pub fn stack_index(&self, index: usize) -> Option<Value> {
    self.stack.get(index).cloned()
  }

  pub fn stack_peek(&self) -> Option<Value> {
    self.stack.last().cloned()
  }

  pub fn stack_assign(&mut self, index: usize, value: Value) {
    self.stack[index] = value;
  }

  pub fn const_at(&self, index: usize) -> Option<Value> {
    self.consts.get(index).cloned()
  }

  pub fn lookup_global(&self, name: &String) -> Option<Value> {
    self.env.lookup(name)
  }

  pub fn define_global(&mut self, name: String, value: Value) -> bool {
    self.env.define(name, value)
  }

  pub fn assign_global(&mut self, name: String, value: Value) -> bool {
    self.env.assign(name, value)
  }

  pub fn jump(&mut self, count: usize) {
    self.ip = self.ip.saturating_add(count);
  }

  pub fn loop_back(&mut self, count: usize) {
    self.ip = self.ip.saturating_sub(count);
  }

  pub fn display_opcodes(&self) {
    println!("<< MAIN >>");
    for (i, op) in self.instructions.iter().enumerate() {
      self.display_instruction(op, i);
    }
    println!("<< END >>");
  }

  pub fn display_instruction(&self, op: &OpCode, offset: usize) {
    print!("0x{:#04X} ", offset);
    if offset > 0 && self.meta.line_at(offset) == self.meta.line_at(offset - 1) {
      print!("   | ");
    } else {
      print!("{:#04} ", self.meta.line_at(offset));
    }

    match op {
      OpCode::Const(index) => {
        print!("{:<16?} {:4} ", op, index);
        let c = self.const_at(*index);
        match c {
          Some(v) => println!("'{}'", v),
          None => println!("INVALID INDEX"),
        }
      }
      OpCode::PopN(count) => println!("{:<16?} {:4}", op, count),
      OpCode::LookupLocal(index) => println!("{:<16?} {:4}", op, index),
      OpCode::AssignLocal(index) => println!("{:<16?} {:4}", op, index),
      OpCode::LookupGlobal(name) => println!("{:<16?} '{}'", op, name),
      OpCode::DefineGlobal(name) => println!("{:<16?} '{}'", op, name),
      OpCode::AssignGlobal(name) => println!("{:<16?} '{}'", op, name),
      OpCode::Jump(count) => println!("{:<16?} {:4}", op, count),
      OpCode::JumpIfFalse(count) => println!("{:<16?} {:4}", op, count),
      OpCode::Loop(count) => println!("{:<16?} {:4}", op, count),
      OpCode::Or(count) => println!("{:<16?} {:4}", op, count),
      OpCode::And(count) => println!("{:<16?} {:4}", op, count),
      x => println!("{:?}", op),
    }
  }
}
