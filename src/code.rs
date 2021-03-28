use crate::types::{Env, Value};
use std::{
  fmt::{self, Debug, Display},
  str,
};

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

#[derive(Debug, PartialEq)]
pub enum Token {
  Invalid,

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
  Loop,
  Match,
  Nil,
  Or,
  Print,
  Ret,
  True,
  While,
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
      x => println!("{:?}", x),
    }
  }
}

struct Scanner<'src> {
  source: &'src str,
  raw_src: &'src [u8],
  start_pos: usize,
  pos: usize,
  line: usize,
  column: usize,
}

impl<'src> Scanner<'src> {
  fn new(source: &'src str) -> Self {
    Scanner {
      source,
      raw_src: source.as_bytes(),
      start_pos: 0,
      pos: 0,
      line: 0,
      column: 0,
    }
  }

  fn scan(&mut self) -> Vec<Token> {
    let mut tokens = Vec::new();

    loop {
      self.skip_whitespace();
      if let Some(c) = self.peek() {
        self.start_pos = self.pos;
        let token = match c {
          '(' => Token::LeftParen,
          ')' => Token::RightParen,
          '{' => Token::LeftBrace,
          '}' => Token::RightBrace,
          ',' => Token::Comma,
          '.' => Token::Dot,
          ';' => Token::Semicolon,
          '+' => Token::Plus,
          '-' => Token::Minus,
          '*' => Token::Asterisk,
          '/' => Token::Slash,
          '%' => Token::Modulus,
          '!' => {
            if self.advance_if_match('=') {
              Token::BangEqual
            } else {
              Token::Bang
            }
          }
          '=' => {
            if self.advance_if_match('=') {
              Token::EqualEqual
            } else if self.advance_if_match('>') {
              Token::Arrow
            } else {
              Token::Equal
            }
          }
          '<' => {
            if self.advance_if_match('=') {
              Token::LessEqual
            } else {
              Token::Less
            }
          }
          '>' => {
            if self.advance_if_match('=') {
              Token::GreaterEqual
            } else {
              Token::Greater
            }
          }
          '"' => self.make_string(),
          c if Self::is_digit(c) => self.make_number(),
          c if Self::is_alpha(c) => self.make_ident(),
          _ => {
            todo!("error out")
          }
        };

        if cfg!(test) {
          println!("made token {:?}", token);
        }

        tokens.push(token);

        self.advance();
      } else {
        break;
      }
    }

    tokens
  }

  fn make_number(&mut self) -> Token {
    while let Some(c) = self.peek() {
      if Self::is_digit(c) {
        self.advance();
      } else {
        break;
      }
    }

    if let Some(c1) = self.peek() {
      if c1 == '.' {
        if let Some(c2) = self.peek_n(1) {
          if Self::is_digit(c2) {
            self.advance(); // advance past the '.'
            self.advance(); // advance past the first digit
            while let Some(c) = self.peek() {
              if Self::is_digit(c) {
                self.advance();
              } else {
                break;
              }
            }
          }
        }
      }
    }

    let lexeme = String::from_utf8_lossy(&self.raw_src[self.start_pos..self.pos]);

    match lexeme.parse() {
      Ok(n) => Token::Number(n),
      Err(e) => unimplemented!("error out: {}", e),
    }
  }

  fn make_string(&mut self) -> Token {
    self.advance(); // skip the first "
    while let Some(c) = self.peek() {
      match c {
        '"' => break,
        '\n' => unimplemented!("err out, multiline strings unsupported"),
        _ => self.advance(),
      }
    }

    if self.at_end() {
      unimplemented!("err out, unterminated string");
    }

    match str::from_utf8(&self.raw_src[self.start_pos + 1..self.pos]) {
      Ok(string) => Token::String(String::from(string)),
      Err(e) => unimplemented!("err out, {}", e),
    }
  }

  fn make_ident(&mut self) -> Token {
    while let Some(c) = self.peek() {
      if Self::is_alphanumeric(c) {
        self.advance();
      } else {
        break;
      }
    }

    self.check_keywords()
  }

  fn create_ident(&self) -> Token {
    match str::from_utf8(&self.raw_src[self.start_pos..self.pos]) {
      Ok(string) => Token::Identifier(String::from(string)),
      Err(e) => unimplemented!("err out, {}", e),
    }
  }

  fn check_keywords(&self) -> Token {
    let do_at_depth = |depth, f: &dyn Fn(usize, char) -> Token| -> Token {
      if let Some(c) = self.index_n(self.start_pos + depth) {
        f(depth + 1, c)
      } else {
        self.create_ident()
      }
    };

    do_at_depth(0, &|d, c0| match c0 {
      'a' => self.check_keyword(d, "nd", Token::And),
      'b' => self.check_keyword(d, "reak", Token::Break),
      'c' => do_at_depth(d, &|d, c1| match c1 {
        'l' => self.check_keyword(d, "ass", Token::Class),
        'o' => self.check_keyword(d, "nt", Token::Cont),
        _ => self.create_ident(),
      }),
      'e' => do_at_depth(d, &|d, c1| match c1 {
        'l' => self.check_keyword(d, "se", Token::Else),
        'n' => self.check_keyword(d, "d", Token::End),
        _ => self.create_ident(),
      }),
      'f' => do_at_depth(d, &|d, c1| match c1 {
        'a' => self.check_keyword(d, "lse", Token::False),
        'n' => self.check_keyword(d, "", Token::Fn),
        'o' => self.check_keyword(d, "r", Token::For),
        _ => self.create_ident(),
      }),
      'i' => self.check_keyword(d, "f", Token::If),
      'l' => do_at_depth(d, &|d, c1| match c1 {
        'e' => self.check_keyword(d, "t", Token::Let),
        'o' => do_at_depth(d, &|d, c2| match c2 {
          'a' => self.check_keyword(d, "d", Token::Load),
          'o' => self.check_keyword(d, "p", Token::Loop),
          _ => self.create_ident(),
        }),
        _ => self.create_ident(),
      }),
      'm' => self.check_keyword(d, "atch", Token::Match),
      'n' => self.check_keyword(d, "il", Token::Nil),
      'o' => self.check_keyword(d, "r", Token::Or),
      'p' => self.check_keyword(d, "rint", Token::Print),
      'r' => self.check_keyword(d, "et", Token::Ret),
      't' => self.check_keyword(d, "rue", Token::True),
      'w' => self.check_keyword(d, "hile", Token::While),
      _ => self.create_ident(),
    })
  }

  fn check_keyword(&self, start: usize, rest: &str, checkee: Token) -> Token {
    let bytes = rest.as_bytes();
    let begin = self.start_pos + start;
    if self.pos - self.start_pos == start + rest.len()
      && &self.raw_src[begin..begin + rest.len()] == bytes
    {
      checkee
    } else {
      self.create_ident()
    }
  }

  fn skip_whitespace(&mut self) {
    while let Some(c) = self.peek() {
      match c {
        '#' => {
          while !self.at_end() {
            if let Some(c) = self.peek() {
              if c == '\n' {
                break;
              } else {
                self.advance();
              }
            } else {
              break;
            }
          }
        }
        '\n' => {
          self.line += 1;
          self.column = 0;
          self.advance();
        }
        c if c == ' ' || c == '\r' || c == '\t' => {
          self.advance();
        }
        _ => break,
      }
    }
  }

  fn at_end(&self) -> bool {
    self.pos >= self.raw_src.len()
  }

  fn peek(&self) -> Option<char> {
    self.raw_src.get(self.pos).map(|c| *c as char)
  }

  fn peek_n(&self, n: usize) -> Option<char> {
    self
      .raw_src
      .get(self.pos.saturating_add(n))
      .map(|c| *c as char)
  }

  fn index_n(&self, n: usize) -> Option<char> {
    self.raw_src.get(n).map(|c| *c as char)
  }

  fn advance(&mut self) {
    self.pos += 1;
  }

  fn advance_if_match(&mut self, expected: char) -> bool {
    match self.peek_n(1) {
      Some(c) => {
        if c == expected {
          self.advance();
          true
        } else {
          false
        }
      }
      None => false,
    }
  }

  fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
  }

  fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '@'
  }

  fn is_alphanumeric(c: char) -> bool {
    Self::is_alpha(c) || Self::is_digit(c)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[cfg(test)]
  mod scanner {
    use super::*;

    const ALL_TOKENS: &str = "( ) { } , . ; + - * / % ! != = == > >= < <= =>
    foobar \"some string\" 3.14159 and break class cont else end false for
    fn if let load loop match nil or print ret true while ";

    #[test]
    fn scanner_scans() {
      let mut scanner = Scanner::new(ALL_TOKENS);
      let actual = scanner.scan();
      let expected = vec![
        Token::LeftParen,
        Token::RightParen,
        Token::LeftBrace,
        Token::RightBrace,
        Token::Comma,
        Token::Dot,
        Token::Semicolon,
        Token::Plus,
        Token::Minus,
        Token::Asterisk,
        Token::Slash,
        Token::Modulus,
        Token::Bang,
        Token::BangEqual,
        Token::Equal,
        Token::EqualEqual,
        Token::Greater,
        Token::GreaterEqual,
        Token::Less,
        Token::LessEqual,
        Token::Arrow,
        Token::Identifier(String::from("foobar")),
        Token::String(String::from("some string")),
        Token::Number(3.14159),
        Token::And,
        Token::Break,
        Token::Class,
        Token::Cont,
        Token::Else,
        Token::End,
        Token::False,
        Token::For,
        Token::Fn,
        Token::If,
        Token::Let,
        Token::Load,
        Token::Loop,
        Token::Match,
        Token::Nil,
        Token::Or,
        Token::Print,
        Token::Ret,
        Token::True,
        Token::While,
      ];
      assert_eq!(actual.len(), expected.len());

      for (t0, t1) in actual.iter().zip(expected.iter()) {
        assert_eq!(t0, t1);
      }
    }
  }
}
