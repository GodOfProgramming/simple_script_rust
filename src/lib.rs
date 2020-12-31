use scanner::Scanner;
use std::fmt::{self, Display};
use std::io::{self, Write};
use types::{Value, ValueArray};

macro_rules! is_debug {
  () => {
    cfg!(debug_assertions)
  };
}

#[derive(Debug)]
pub struct ScriptError {
  pub line: usize,
  pub msg: String,
}

impl Display for ScriptError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} ({}): {}", self.file, self.line, self.msg)
  }
}

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
  Compile(ScriptError),
  Runtime(ScriptError),
}

impl Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Compile(se) => write!(f, "compile error: {}", se),
      Self::Runtime(se) => write!(f, "runtime error: {}", se),
    }
  }
}

pub type VMResult = std::result::Result<(), Error>;

pub struct VM {}

impl VM {
  pub fn run_script(&mut self, script: &str) -> VMResult {
    self.compile(script)
  }

  fn compile(&mut self, _script: &str) -> VMResult {
    let scanner = Scanner::default();
    let mut last_line: Option<usize> = None;
    while let Some(token) = scanner.scan() {
      if let Some(line) = last_line {
        if token.line != line {
          print!("{:04} ", token.line);
          last_line = Some(token.line);
        } else {
          print!("   | ")
        }
      } else {
        print!("{:04} ", token.line);
        last_line = Some(token.line);
      }

      println!("{:02} {}", token.kind, token);
    }
    Ok(())
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
          let res = self.operate_on(&chunk, ip, "add", &mut stack, |a, b| a + b)?;
          stack.push(res);
        }
        OpCode::Subtract => {
          let res = self.operate_on(&chunk, ip, "subtract", &mut stack, |a, b| a - b)?;
          stack.push(res);
        }
        OpCode::Multiply => {
          let res = self.operate_on(&chunk, ip, "multiply", &mut stack, |a, b| a * b)?;
          stack.push(res);
        }
        OpCode::Divide => {
          let res = self.operate_on(&chunk, ip, "divide", &mut stack, |a, b| a / b)?;
          stack.push(res);
        }
        OpCode::Negate => {
          if let Some(v) = stack.pop() {
            stack.push(-v);
          } else {
            return Err(Error::Runtime(ScriptError {
              file: chunk.name,
              line: chunk.line_at(ip),
              msg: String::from("tried to negate a void value"),
            })); // tried to negate a void value
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

  pub fn cli(&mut self) -> bool {
    let mut input = String::new();
    let mut exit = false;
    let mut line_number = 1;

    while !exit {
      input.clear();
      print!("ss(main):{}> ", line_number);
      if let Err(err) = io::stdout().flush() {
        println!("{}", err);
        return false;
      }

      if let Err(err) = io::stdin().read_line(&mut input) {
        println!("{}", err);
        return false;
      }

      if input == "exit" {
        exit = true;
      }

      if let Err(e) = self.run_script(&input) {
        println!("{}", e);
      }
    }

    true
  }

  fn operate_on(
    &mut self,
    chunk: &Chunk,
    offset: usize,
    operator: &str,
    stack: &mut Vec<Value>,
    f: fn(a: Value, b: Value) -> Value,
  ) -> Result<Value, Error> {
    if let Some(a) = stack.pop() {
      if let Some(b) = stack.pop() {
        Ok(f(a, b))
      } else {
        Err(Error::Runtime(ScriptError {
          file: chunk.name,
          line: chunk.line_at(offset),
          msg: format!("cannot {} with {} and void", operator, a),
        }))
      }
    } else {
      Err(Error::Runtime(ScriptError {
        file: chunk.name,
        line: chunk.line_at(offset),
        msg: format!("cannot {} with void", operator),
      }))
    }
  }
}

impl Default for VM {
  fn default() -> Self {
    Self {}
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

mod scanner {
  use super::ScriptError;
  use std::iter::{Peekable, Skip};

  pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Minus,
    Plus,
    Slash,
    Asterisk,
    Semicolon,
    BackSlash,
    Conditional,
    Colon,
    Pipe,

    // One or two character tokens.
    Exclamation,
    ExEq,
    Equal,
    EqEq,
    GreaterThan,
    GreaterEq,
    LessThan,
    LessEq,
    Dot,
    Range,

    // Literals.
    Identifier,
    StringLiteral,
    NumberLiteral,

    // Keywords.
    And,
    Bool,
    Class,
    Else,
    Error,
    False,
    Fn,
    For,
    If,
    Is,
    Let,
    List,
    Nil,
    Number,
    Or,
    Print,
    Return,
    String,
    True,
    While,

    EOF,
  }

  pub struct Token<'lexeme> {
    kind: TokenKind,
    lexeme: &'lexeme str,
    line: usize,
  }

  impl<'lexeme> Token<'lexeme> {
    fn new(kind: TokenKind, lexeme: &'lexeme str, line: usize) -> Token {
      Token { kind, lexeme, line }
    }
  }

  pub struct Scanner<'src> {
    start: usize,
    current: usize,
    line: usize,

    src: &'src str,
  }

  impl<'src> Scanner<'src> {
    fn new(src: &'src str) -> Self {
      Self {
        start: 0,
        current: 0,
        line: 1,
        src,
      }
    }

    fn scan(&mut self) -> Result<Token, ScriptError> {
      let mut chars = self.src.chars().skip(self.current).peekable();
      self.skip_nontokens(&mut chars);
      self.start = self.current;
      match self.advance(&mut chars) {
        Some(c) => match c {
          '(' => Ok(self.make_token(TokenKind::LeftParen)),
          ')' => Ok(self.make_token(TokenKind::RightParen)),
          '{' => Ok(self.make_token(TokenKind::LeftBrace)),
          '}' => Ok(self.make_token(TokenKind::RightBrace)),
          ';' => Ok(self.make_token(TokenKind::Semicolon)),
          ',' => Ok(self.make_token(TokenKind::Comma)),
          '.' => Ok(self.make_token(TokenKind::Dot)),
          '-' => Ok(self.make_token(TokenKind::Minus)),
          '+' => Ok(self.make_token(TokenKind::Plus)),
          '*' => Ok(self.make_token(TokenKind::Asterisk)),
          '/' => Ok(self.make_token(TokenKind::Slash)),
          '!' => {
            if self.advance_if_matches('=', &mut chars) {
              Ok(self.make_token(TokenKind::ExEq))
            } else {
              Ok(self.make_token(TokenKind::Exclamation))
            }
          }
          '=' => {
            if self.advance_if_matches('=', &mut chars) {
              Ok(self.make_token(TokenKind::EqEq))
            } else {
              Ok(self.make_token(TokenKind::Equal))
            }
          }
          '<' => {
            if self.advance_if_matches('=', &mut chars) {
              Ok(self.make_token(TokenKind::LessEq))
            } else {
              Ok(self.make_token(TokenKind::LessThan))
            }
          }
          '>' => {
            if self.advance_if_matches('=', &mut chars) {
              Ok(self.make_token(TokenKind::GreaterEq))
            } else {
              Ok(self.make_token(TokenKind::GreaterThan))
            }
          }
          '"' => self.make_string(&mut chars),
          _ if Scanner::is_digit(c) => self.make_number(&mut chars),
          _ => Err(ScriptError {
            line: self.line,
            msg: format!("unexpected character"),
          }),
        },
        None => Ok(self.make_token(TokenKind::EOF)),
      }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
      Token::new(kind, &self.src[self.start..self.current], self.line)
    }

    fn advance(&mut self, chars: &mut Peekable<Skip<std::str::Chars>>) -> Option<char> {
      self.current += 1;
      chars.next()
    }

    fn advance_if_matches(
      &mut self,
      expected: char,
      chars: &mut Peekable<Skip<std::str::Chars>>,
    ) -> bool {
      match chars.peek() {
        Some(c) => {
          if *c == expected {
            self.advance(chars);
            true
          } else {
            false
          }
        }
        None => false,
      }
    }

    fn skip_nontokens(&mut self, chars: &mut Peekable<Skip<std::str::Chars>>) {
      while let Some(c) = chars.peek() {
        match c {
          ' ' | '\r' | '\t' => {
            self.advance(chars);
          }
          '\n' => {
            self.line += 1;
            self.advance(chars);
          }
          '#' => {
            while let Some(c) = self.advance(chars) {
              if c == '\n' {
                self.line += 1;
                break;
              }
            }
          }
          _ => return,
        }
      }
    }

    fn make_string(
      &mut self,
      chars: &mut Peekable<Skip<std::str::Chars>>,
    ) -> Result<Token, ScriptError> {
      while let Some(c) = self.advance(chars) {
        match c {
          '\n' => self.line += 1,
          '"' => return Ok(self.make_token(TokenKind::StringLiteral)),
        }
      }

      Err(ScriptError {
        line: self.line,
        msg: String::from("unterminated string"),
      })
    }

    fn is_digit(c: char) -> bool {
      c >= '0' && c <= '9'
    }

    fn peek_next(chars: &mut Peekable<Skip<std::str::Chars>>, skips: usize) -> Option<char> {
      chars.clone().skip(skips).next()
    }

    fn make_number(
      &mut self,
      chars: &mut Peekable<Skip<std::str::Chars>>,
    ) -> Result<Token, ScriptError> {
      while let Some(c) = self.advance(chars) {
        if !Scanner::is_digit(c) {
          break;
        }
      }

      if let Some(c) = chars.peek() {
        if *c == '.' {
          if let Some(c) = Scanner::peek_next(chars, 1) {
            self.advance(chars);
            while let Some(c) = self.advance(chars) {
              if !Scanner::is_digit(c) {
                break;
              }
            }
          }
        }
      }

      Ok(self.make_token(TokenKind::Number))
    }
  }
}

mod types {
  use super::*;
  pub use call::{Airity, Function, NativeFn};
  pub use oo::{Class, Instance};
  pub use value::{Value, ValueArray};

  trait New<T> {
    fn new(item: T) -> Self;
  }

  trait ValueError<T> {
    fn new_err(err: T) -> Self;
  }

  mod value {
    use super::{Class, Function, Instance, New, ValueError};
    use std::cmp::{Ordering, PartialEq, PartialOrd};
    use std::fmt::{self, Debug, Display};
    use std::ops::{
      Add, AddAssign, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Not, Rem, RemAssign,
      Sub, SubAssign,
    };

    #[derive(Clone)]

    pub struct ValueArray {
      values: Vec<Value>,
    }

    impl ValueArray {
      pub fn new() -> Self {
        Self { values: Vec::new() }
      }

      // returns the location of the added constant
      pub fn write(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
      }
    }

    impl Index<usize> for ValueArray {
      type Output = Value;
      fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
      }
    }

    #[derive(Clone)]
    pub enum Value {
      Nil,
      Error(Box<Value>),
      Bool(bool),
      Num(f64),
      Str(String),
      List(Values),
      Callee(Function),
      Class(Class),
      Instance(Instance),
    }

    impl Value {
      pub fn truthy(&self) -> bool {
        if *self == Value::Nil {
          return false;
        }

        if let Value::Bool(b) = self {
          return *b;
        }

        true
      }

      pub fn len(&self) -> Option<usize> {
        if let Self::List(list) = self {
          Some(list.len())
        } else {
          None
        }
      }

      pub fn is_empty(&self) -> Option<bool> {
        if let Self::List(list) = self {
          Some(list.is_empty())
        } else {
          None
        }
      }

      pub fn index(&'_ self, idx: usize) -> Option<&'_ Self> {
        if let Self::List(list) = self {
          Some(&list[idx])
        } else {
          None
        }
      }

      pub fn index_mut(&'_ mut self, idx: usize) -> Option<&'_ mut Self> {
        if let Self::List(list) = self {
          Some(&mut list[idx])
        } else {
          None
        }
      }
    }

    impl New<bool> for Value {
      fn new(item: bool) -> Self {
        Self::Bool(item)
      }
    }

    impl New<f64> for Value {
      fn new(item: f64) -> Self {
        Self::Num(item)
      }
    }

    impl New<String> for Value {
      fn new(item: String) -> Self {
        Self::Str(item)
      }
    }

    impl New<&str> for Value {
      fn new(item: &str) -> Self {
        Self::new(String::from(item))
      }
    }

    impl New<Values> for Value {
      fn new(item: Values) -> Self {
        Self::List(item)
      }
    }

    impl New<Function> for Value {
      fn new(item: Function) -> Self {
        Self::Callee(item)
      }
    }

    impl New<Class> for Value {
      fn new(item: Class) -> Self {
        Self::Class(item)
      }
    }

    impl New<Instance> for Value {
      fn new(item: Instance) -> Self {
        Self::Instance(item)
      }
    }

    impl ValueError<bool> for Value {
      fn new_err(err: bool) -> Self {
        Self::Error(Box::new(Self::Bool(err)))
      }
    }

    impl ValueError<f64> for Value {
      fn new_err(err: f64) -> Self {
        Self::Error(Box::new(Self::Num(err)))
      }
    }

    impl ValueError<&str> for Value {
      fn new_err(err: &str) -> Self {
        Self::new_err(String::from(err))
      }
    }

    impl ValueError<String> for Value {
      fn new_err(err: String) -> Self {
        Self::Error(Box::new(Self::Str(err)))
      }
    }

    impl ValueError<Values> for Value {
      fn new_err(err: Values) -> Self {
        Self::Error(Box::new(Self::List(err)))
      }
    }

    impl ValueError<Function> for Value {
      fn new_err(err: Function) -> Self {
        Self::Error(Box::new(Self::Callee(err)))
      }
    }

    impl ValueError<Class> for Value {
      fn new_err(err: Class) -> Self {
        Self::Error(Box::new(Self::Class(err)))
      }
    }

    impl ValueError<Instance> for Value {
      fn new_err(err: Instance) -> Self {
        Self::Error(Box::new(Self::Instance(err)))
      }
    }

    impl Add for Value {
      type Output = Self;
      fn add(self, other: Self) -> Self {
        match self {
          Self::Num(a) => match other {
            Self::Num(b) => Self::Num(a + b),
            Self::Str(b) => Self::Str(format!("{}{}", a, b)),
            _ => Self::new_err(format!("cannot add {} and {}", a, other)),
          },
          Self::Str(a) => match other {
            Self::Num(b) => Self::Str(format!("{}{}", a, b)),
            Self::Str(b) => Self::Str(format!("{}{}", a, b)),
            _ => Self::new_err(format!("cannot add {} and {}", a, other)),
          },
          _ => Self::new_err(format!("cannot add {} and {}", self, other)),
        }
      }
    }

    impl AddAssign for Value {
      fn add_assign(&mut self, other: Self) {
        *self = self.clone() + other;
      }
    }

    impl Sub for Value {
      type Output = Self;

      fn sub(self, other: Self) -> Self::Output {
        match self {
          Self::Num(a) => match other {
            Self::Num(b) => Self::Num(a - b),
            _ => Self::new_err(format!("cannot sub {} and {}", a, other)),
          },
          _ => Self::new_err(format!("cannot sub {} and {}", self, other)),
        }
      }
    }

    impl SubAssign for Value {
      fn sub_assign(&mut self, other: Self) {
        *self = self.clone() - other;
      }
    }

    impl Mul for Value {
      type Output = Self;

      fn mul(self, other: Self) -> Self::Output {
        match self {
          Self::Num(a) => match other {
            Self::Num(b) => Self::Num(a * b),
            Self::Str(b) => {
              if a > 0.0 {
                Self::Str(b.repeat(a as usize))
              } else {
                Self::new_err(format!("cannot repeat a string {} times", b))
              }
            }
            _ => Self::new_err(format!("cannot multiply {} and {}", a, other)),
          },
          Self::Str(a) => match other {
            Self::Num(b) => {
              if b > 0.0 {
                Self::new(a.repeat(b as usize))
              } else {
                Self::new_err(format!("cannot repeat a string {} times", a))
              }
            }
            _ => Self::new_err(format!("cannot multiply {} and {}", a, other)),
          },
          _ => Self::new_err(format!("cannot multiply {} and {}", self, other)),
        }
      }
    }

    impl MulAssign for Value {
      fn mul_assign(&mut self, other: Self) {
        *self = self.clone() * other;
      }
    }

    impl Div for Value {
      type Output = Self;

      fn div(self, other: Self) -> Self::Output {
        match self {
          Self::Num(a) => match other {
            Self::Num(b) => Self::new(a / b),
            _ => Self::new_err(format!("cannot divide {} by {}", self, other)),
          },
          _ => Self::new_err(format!("cannot divide {} by {}", self, other)),
        }
      }
    }

    impl DivAssign for Value {
      fn div_assign(&mut self, other: Self) {
        *self = self.clone() / other;
      }
    }

    impl Rem for Value {
      type Output = Self;

      fn rem(self, other: Self) -> Self::Output {
        match self {
          Self::Num(a) => match other {
            Self::Num(b) => Self::new(a % b),
            _ => Self::new_err(format!("cannot modulus {} by {}", self, other)),
          },
          _ => Self::new_err(format!("cannot modulus {} by {}", self, other)),
        }
      }
    }

    impl RemAssign for Value {
      fn rem_assign(&mut self, other: Self) {
        *self = self.clone() % other;
      }
    }

    impl Not for Value {
      type Output = Self;

      fn not(self) -> Self::Output {
        Value::Bool(!self.truthy())
      }
    }

    impl Neg for Value {
      type Output = Self;

      fn neg(self) -> Self::Output {
        match self {
          Self::Nil => Self::new_err("cannot negate nil"),
          Self::Error(_) => Self::new_err("cannot negate an error"),
          Self::Bool(_) => Self::new_err("cannot negate a bool"),
          Self::Num(n) => Self::Num(-n),
          Self::Str(_) => Self::new_err("cannot negate a string"),
          Self::List(_) => Self::new_err("cannot negate a list"),
          Self::Callee(_) => Self::new_err("cannot negate a function"),
          Self::Class(_) => Self::new_err("cannot negate a class"),
          Self::Instance(i) => Self::new_err(format!("cannot negate a {}", i.instance_of)),
        }
      }
    }

    impl PartialEq for Value {
      fn eq(&self, other: &Self) -> bool {
        match self {
          Self::Error(a) => {
            if let Self::Error(b) = other {
              a == b
            } else {
              false
            }
          }
          Self::Bool(a) => {
            if let Self::Bool(b) = other {
              a == b
            } else {
              false
            }
          }
          Self::Str(a) => {
            if let Self::Str(b) = other {
              a == b
            } else {
              false
            }
          }
          Self::Num(a) => {
            if let Self::Num(b) = other {
              a == b
            } else {
              false
            }
          }
          Self::List(a) => {
            if let Self::List(b) = other {
              let a = &a.0;
              let b = &b.0;

              if a.len() == b.len() {
                for it in a.iter().zip(b.iter()) {
                  let (ai, bi) = it;
                  if ai != bi {
                    return false;
                  }
                }

                true
              } else {
                false
              }
            } else {
              false
            }
          }
          Self::Callee(_) => panic!("comparing functions is unimplemented"),
          Self::Class(a) => {
            if let Self::Class(b) = other {
              a == b
            } else {
              false
            }
          }
          Self::Instance(a) => {
            if let Self::Instance(b) = other {
              a == b
            } else {
              false
            }
          }
          Self::Nil => {
            matches!(other, Value::Nil)
          }
        }
      }
    }

    impl PartialOrd for Value {
      fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
          Self::Num(a) => match other {
            Self::Num(b) => {
              if a < b {
                Some(Ordering::Less)
              } else if a > b {
                Some(Ordering::Greater)
              } else if (a - b).abs() < f64::EPSILON {
                Some(Ordering::Equal)
              } else {
                None
              }
            }
            _ => None,
          },
          Self::Str(a) => match other {
            Self::Str(b) => Some(a.cmp(&b)),
            _ => None,
          },
          _ => None,
        }
      }
    }

    impl Display for Value {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
          Self::Nil => write!(f, "nil"),
          Self::Error(e) => write!(f, "{}", e),
          Self::Bool(b) => write!(f, "{}", b),
          Self::Num(n) => write!(f, "{}", n),
          Self::Str(s) => write!(f, "{}", s),
          Self::List(l) => write!(f, "{}", l),
          Self::Callee(c) => write!(f, "{}", c),
          Self::Class(c) => write!(f, "<class {}>", c),
          Self::Instance(i) => write!(f, "<instance of {}>", i),
        }
      }
    }

    impl Debug for Value {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
      }
    }

    #[derive(Clone)]
    pub struct Values(Vec<Value>);

    impl Values {
      pub fn new(values: Vec<Value>) -> Self {
        Self(values)
      }

      pub fn len(&self) -> usize {
        self.0.len()
      }

      pub fn is_empty(&self) -> bool {
        self.0.is_empty()
      }
    }

    impl Index<usize> for Values {
      type Output = Value;

      fn index(&self, idx: usize) -> &Self::Output {
        &self.0[idx]
      }
    }

    impl IndexMut<usize> for Values {
      fn index_mut(&mut self, idx: usize) -> &mut Value {
        &mut self.0[idx]
      }
    }

    impl IntoIterator for Values {
      type Item = Value;
      type IntoIter = std::vec::IntoIter<Self::Item>;

      fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
      }
    }

    impl Display for Values {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, value| {
          result.and_then(|_| writeln!(f, "{}", value))
        })
      }
    }

    #[cfg(test)]
    mod tests {
      use super::*;
      use crate::env::EnvRef;
      use crate::types::Airity;

      #[test]
      fn is_truthy() {
        let true_expectations = |t: Value| {
          assert!(t.truthy());
        };

        let false_expectations = |t: Value| {
          assert!(!t.truthy());
        };

        run_assertions(
          vec![true_expectations],
          vec![
            Value::new_err("can be any error type"),
            Value::new(true),
            Value::new(0.0),
            Value::new(1.0),
            Value::new(-1.0),
            Value::new("some string"),
            Value::new(Values::new(Vec::new())),
            // TODO
            // Value::new(Function::new_native(
            //   String::from("example"),
            //   Airity::Fixed(0),
            //   |_, _| Ok(Value::Nil),
            // )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );

        run_assertions(
          vec![false_expectations],
          vec![Value::Nil, Value::new(false)],
        );
      }

      fn run_assertions(funcs: Vec<fn(Value)>, values: Vec<Value>) {
        for func in funcs {
          let values = values.clone();
          for value in values {
            func(value);
          }
        }
      }

      #[test]
      fn can_add() {
        let x = Value::new(1.0);
        let y = Value::new(2.0);

        assert_eq!(x + y, Value::new(3.0));

        let x = Value::new("x");
        let y = Value::new("y");

        assert_eq!(x + y, Value::new("xy"));

        let x = Value::new(1.0);
        let y = Value::new("y");

        assert_eq!(x + y, Value::new("1y"));

        let x = Value::new("x");
        let y = Value::new(2.0);

        assert_eq!(x + y, Value::new("x2"));
      }

      #[test]
      fn cannot_add_invalid() {
        let assert_err_with_num = |t: Value| {
          let num = Value::new(1.0);
          assert!(matches!(num + t.clone(), Value::Error(_)));
          let num = Value::new(1.0);
          assert!(matches!(t + num, Value::Error(_)));
        };

        let assert_err_with_str = |t: Value| {
          let s = Value::new("a");
          assert!(matches!(s + t.clone(), Value::Error(_)));
          let s = Value::new("a");
          assert!(matches!(t + s, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num, assert_err_with_str],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_add_assign() {
        let mut x = Value::new(1.0);
        let y = Value::new(2.0);
        x += y;

        assert_eq!(x, Value::new(3.0));

        let mut x = Value::new("x");
        let y = Value::new("y");
        x += y;

        assert_eq!(x, Value::new("xy"));

        let mut x = Value::new(1.0);
        let y = Value::new("y");
        x += y;

        assert_eq!(x, Value::new("1y"));

        let mut x = Value::new("x");
        let y = Value::new(2.0);
        x += y;

        assert_eq!(x, Value::new("x2"));
      }

      #[test]
      fn cannot_add_assign_invalid() {
        let assert_err_with_num = |mut t: Value| {
          let mut num = Value::new(1.0);
          num += t.clone();
          assert!(matches!(num, Value::Error(_)));
          let num = Value::new(1.0);
          t += num;
          assert!(matches!(t, Value::Error(_)));
        };

        let assert_err_with_str = |mut t: Value| {
          let mut num = Value::new("a");
          num += t.clone();
          assert!(matches!(num, Value::Error(_)));
          let num = Value::new("a");
          t += num;
          assert!(matches!(t, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num, assert_err_with_str],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_sub() {
        let x = Value::new(3.0);
        let y = Value::new(2.0);

        assert_eq!(x - y, Value::new(1.0));
      }

      #[test]
      fn cannot_sub_invalid() {
        let assert_err_with_num = |t: Value| {
          let num = Value::new(1.0);
          assert!(matches!(num - t.clone(), Value::Error(_)));
          let num = Value::new(1.0);
          assert!(matches!(t - num, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_sub_assign() {
        let mut x = Value::new(3.0);
        let y = Value::new(2.0);
        x -= y;

        assert_eq!(x, Value::new(1.0));
      }

      #[test]
      fn cannot_sub_assign_invalid() {
        let assert_err_with_num = |mut t: Value| {
          let mut num = Value::new(1.0);
          num -= t.clone();
          assert!(matches!(num, Value::Error(_)));
          let num = Value::new(1.0);
          t -= num;
          assert!(matches!(t, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_mul() {
        let x = Value::new(2.0);
        let y = Value::new(3.0);

        assert_eq!(x * y, Value::new(6.0));

        let x = Value::new(2.0);
        let y = Value::new("a");

        assert_eq!(x * y, Value::new("aa"));

        let x = Value::new(2.0);
        let y = Value::new("a");

        assert_eq!(x * y, Value::new("aa"));
      }

      #[test]
      fn cannot_mul_invalid() {
        let assert_err_with_num = |t: Value| {
          let num = Value::new(1.0);
          assert!(matches!(num * t.clone(), Value::Error(_)));
          let num = Value::new(1.0);
          assert!(matches!(t * num, Value::Error(_)));
        };

        let assert_err_with_str = |t: Value| {
          let s = Value::new("a");
          assert!(matches!(s * t.clone(), Value::Error(_)));
          let s = Value::new("a");
          assert!(matches!(t * s, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num, assert_err_with_str],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );

        run_assertions(
          vec![assert_err_with_str],
          vec![Value::new(-1.0), Value::new("test")],
        );
      }

      #[test]
      fn can_mul_assign() {
        let mut x = Value::new(2.0);
        let y = Value::new(3.0);
        x *= y;

        assert_eq!(x, Value::new(6.0));

        let mut x = Value::new(2.0);
        let y = Value::new("a");
        x *= y;

        assert_eq!(x, Value::new("aa"));

        let mut x = Value::new(2.0);
        let y = Value::new("a");
        x *= y;

        assert_eq!(x, Value::new("aa"));
      }

      #[test]
      fn cannot_mul_assign_invalid() {
        let assert_err_with_num = |mut t: Value| {
          let mut num = Value::new(1.0);
          num *= t.clone();
          assert!(matches!(num, Value::Error(_)));
          let num = Value::new(1.0);
          t *= num;
          assert!(matches!(t, Value::Error(_)));
        };

        let assert_err_with_str = |mut t: Value| {
          let mut s = Value::new("a");
          s *= t.clone();
          assert!(matches!(s, Value::Error(_)));
          let s = Value::new("a");
          t *= s;
          assert!(matches!(t, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num, assert_err_with_str],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );

        run_assertions(
          vec![assert_err_with_str],
          vec![Value::new(-1.0), Value::new("test")],
        );
      }

      #[test]
      fn can_div() {
        let x = Value::new(3.0);
        let y = Value::new(2.0);

        assert_eq!(x / y, Value::new(1.5));
      }

      #[test]
      fn cannot_div_invalid() {
        let assert_err_with_num = |t: Value| {
          let num = Value::new(1.0);
          assert!(matches!(num / t.clone(), Value::Error(_)));
          let num = Value::new(1.0);
          assert!(matches!(t / num, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_div_assign() {
        let mut x = Value::new(3.0);
        let y = Value::new(2.0);
        x /= y;

        assert_eq!(x, Value::new(1.5));
      }

      #[test]
      fn cannot_div_assign_invalid() {
        let assert_err_with_num = |mut t: Value| {
          let mut num = Value::new(1.0);
          num /= t.clone();
          assert!(matches!(num, Value::Error(_)));
          let num = Value::new(1.0);
          t /= num;
          assert!(matches!(t, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_mod() {
        let x = Value::new(3.0);
        let y = Value::new(2.0);

        assert_eq!(x % y, Value::new(1.0));
      }

      #[test]
      fn cannot_mod_invalid() {
        let assert_err_with_num = |t: Value| {
          let num = Value::new(1.0);
          assert!(matches!(num % t.clone(), Value::Error(_)));
          let num = Value::new(1.0);
          assert!(matches!(t % num, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_mod_assign() {
        let mut x = Value::new(3.0);
        let y = Value::new(2.0);
        x %= y;

        assert_eq!(x, Value::new(1.0));
      }

      #[test]
      fn cannot_mod_assign_invalid() {
        let assert_err_with_num = |mut t: Value| {
          let mut num = Value::new(1.0);
          num %= t.clone();
          assert!(matches!(num, Value::Error(_)));
          let num = Value::new(1.0);
          t %= num;
          assert!(matches!(t, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err_with_num],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn not_a_value_returns_opposite_truthiness() {
        let true_expectations = |t: Value| {
          assert_eq!(Value::new(true), !t);
        };

        let false_expectations = |t: Value| {
          assert_eq!(Value::new(false), !t);
        };

        run_assertions(vec![true_expectations], vec![Value::Nil, Value::new(false)]);

        run_assertions(
          vec![false_expectations],
          vec![
            Value::new_err("can be any error type"),
            Value::new(true),
            Value::new(0.0),
            Value::new(1.0),
            Value::new(-1.0),
            Value::new("some string"),
            Value::new(Values::new(Vec::new())),
            // TODO
            // Value::new(Function::new_native(
            //   String::from("example"),
            //   Airity::Fixed(0),
            //   |_, _| Ok(Value::Nil),
            // )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }

      #[test]
      fn can_negate() {
        let x = Value::new(1.0);
        assert_eq!(-x, Value::new(-1.0));
      }

      #[test]
      fn cannot_negate_invalid() {
        let assert_err = |t: Value| {
          assert!(matches!(-t, Value::Error(_)));
        };

        run_assertions(
          vec![assert_err],
          vec![
            Value::Nil,
            Value::new_err("test error"),
            Value::new(true),
            Value::new(false),
            Value::new("test"),
            Value::new(Values(Vec::new())),
            Value::new(Function::new_native(
              String::from("example"),
              Airity::Fixed(0),
              |_, _| Ok(Value::Nil),
            )),
            Value::new(Class::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
            Value::new(Instance::new(
              String::from("example"),
              EnvRef::default(),
              EnvRef::default(),
            )),
          ],
        );
      }
    }
  }

  mod call {
    use super::scanner::Token;
    use super::{Value, VM};
    use crate::env::EnvRef;
    use crate::ScriptError;
    use std::fmt::{self, Display};
    use std::ops::RangeInclusive;
    use std::rc::Rc;

    pub type NativeFnResult = Result<Value, String>;
    pub type NativeFn = fn(EnvRef, &[Value]) -> NativeFnResult;

    #[derive(Clone)]
    pub enum Airity {
      Fixed(usize),
      Range(RangeInclusive<usize>),
    }

    #[derive(Clone)]
    pub enum Function {
      Native {
        name: String,
        airity: Airity,
        func: NativeFn,
      },
      Script {
        name: String,
        params: Rc<Vec<Token>>,
        body: Rc<Vec<Stmt>>,
        env: EnvRef,
      },
      Closure {
        params: Rc<Vec<Token>>,
        body: Rc<Vec<Stmt>>,
        env: EnvRef,
      },
      Method {
        name: String,
        params: Rc<Vec<Token>>,
        body: Rc<Vec<Stmt>>,
        env: EnvRef,
      },
    }

    pub type CallResult = Result<Value, ScriptError>;

    impl Function {
      pub fn call(&self, evaluator: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult {
        match self {
          Function::Native {
            name: _,
            airity,
            func,
          } => Function::call_native_fn(evaluator, line, airity, func, &args),
          Function::Script {
            name: _,
            params,
            body,
            env,
          } => Function::call_script_fn(
            evaluator,
            line,
            params,
            body,
            args,
            EnvRef::new_with_enclosing(env.snapshot()),
          ),
          Function::Closure { params, body, env } => Function::call_closure_fn(
            evaluator,
            line,
            params,
            body,
            args,
            EnvRef::new_with_enclosing(env.snapshot()),
          ),
          Function::Method {
            name: _,
            params,
            body,
            env,
          } => Function::call_method(
            evaluator,
            line,
            params,
            body,
            args,
            EnvRef::new_with_enclosing(env.snapshot()),
          ),
        }
      }

      pub fn new_native(name: String, airity: Airity, func: NativeFn) -> Self {
        Self::Native { name, airity, func }
      }

      pub fn new_script(
        name: String,
        params: Rc<Vec<Token>>,
        body: Rc<Vec<Stmt>>,
        env: EnvRef,
      ) -> Self {
        Self::Script {
          name,
          params,
          body,
          env,
        }
      }

      pub fn new_closure(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>, env: EnvRef) -> Self {
        Self::Closure { params, body, env }
      }

      pub fn new_method(
        name: String,
        params: Rc<Vec<Token>>,
        body: Rc<Vec<Stmt>>,
        env: EnvRef,
      ) -> Self {
        Self::Method {
          name,
          params,
          body,
          env,
        }
      }

      fn call_native_fn(
        e: &mut Evaluator,
        line: usize,
        airity: &Airity,
        func: &NativeFn,
        args: &[Value],
      ) -> CallResult {
        match airity {
          Airity::Fixed(len) => {
            if *len < args.len() {
              return Err(ScriptError {
                file: e.file.clone(),
                line,
                msg: format!("too many arguments, expected {}, got {}", len, args.len()),
              });
            }
            if *len > args.len() {
              return Err(ScriptError {
                file: e.file.clone(),
                line,
                msg: format!("too few arguments, expected {}, got {}", len, args.len(),),
              });
            }
          }
          Airity::Range(range) => {
            if !range.contains(&args.len()) {
              return Err(ScriptError {
                file: e.file.clone(),
                line,
                msg: format!(
                  "invalid number of arguments, expected range {:?}, got {}",
                  range,
                  args.len(),
                ),
              });
            }
          }
        }

        match (func)(e.env.snapshot(), args) {
          Ok(v) => Ok(v),
          Err(msg) => Err(ScriptError {
            file: e.file.clone(),
            line,
            msg,
          }),
        }
      }

      fn call_script_fn(
        e: &mut Evaluator,
        line: usize,
        params: &[Token],
        body: &[Stmt],
        args: Vec<Value>,
        mut env: EnvRef,
      ) -> CallResult {
        if params.len() < args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "too many arguments, expected {}, got {}",
              params.len(),
              args.len()
            ),
          });
        }

        if params.len() > args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "too few arguments, expected {}, got {}",
              params.len(),
              args.len(),
            ),
          });
        }

        for (param, arg) in params.iter().zip(args.iter()) {
          env.define(&param.lexeme, arg.clone());
        }

        Ok(match e.eval_block(&body, env)? {
          StatementType::Regular(v) => v,
          StatementType::Return(v) => v,
        })
      }

      fn call_closure_fn(
        e: &mut Evaluator,
        line: usize,
        params: &[Token],
        body: &[Stmt],
        args: Vec<Value>,
        mut env: EnvRef,
      ) -> CallResult {
        if params.len() < args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "too many arguments, expected {}, got {}",
              params.len(),
              args.len()
            ),
          });
        }

        if params.len() > args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "too few arguments, expected {}, got {}",
              params.len(),
              args.len(),
            ),
          });
        }

        for (param, arg) in params.iter().zip(args.iter()) {
          env.define(&param.lexeme, arg.clone());
        }

        Ok(match e.eval_block(&body, env)? {
          StatementType::Regular(v) => v,
          StatementType::Return(v) => v,
        })
      }

      fn call_method(
        e: &mut Evaluator,
        line: usize,
        params: &[Token],
        body: &[Stmt],
        args: Vec<Value>,
        mut env: EnvRef,
      ) -> CallResult {
        if params.is_empty() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: String::from("need at least one parameter for reference to self"),
          });
        }

        if params.len() < args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "too many arguments, expected {}, got {}",
              params.len(),
              args.len()
            ),
          });
        }

        if params.len() - 1 > args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "too few arguments, expected {}, got {}",
              params.len() - 1,
              args.len(),
            ),
          });
        }

        if let Some(self_ref) = params.first() {
          if let Some(instance) = &e.last_object {
            if let Value::Instance(instance) = instance {
              env.define(
                &self_ref.lexeme,
                Value::Instance(Instance {
                  instance_of: instance.instance_of.clone(),
                  methods: instance.methods.snapshot(),
                  members: instance.members.snapshot(),
                }),
              );
            } else {
              return Err(ScriptError {
                file: e.file.clone(),
                line,
                msg: String::from("calling method on non-objects is not allowed"),
              });
            }
          } else {
            return Err(ScriptError {
              file: e.file.clone(),
              line,
              msg: String::from("method called on void space"),
            });
          }
        } else {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: String::from("need at least one parameter for reference to self"),
          });
        }

        for (param, arg) in params.iter().skip(1).zip(args.iter()) {
          env.define(&param.lexeme, arg.clone());
        }

        Ok(match e.eval_block(&body, env.snapshot())? {
          StatementType::Regular(v) => v,
          StatementType::Return(v) => v,
        })
      }
    }

    impl Display for Function {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
          Self::Native {
            name,
            airity: _,
            func: _,
          } => write!(f, "<nf {}>", name),
          Self::Script {
            name,
            params: _,
            body: _,
            env: _,
          } => write!(f, "<fn {}>", name),
          Self::Closure {
            params: _,
            body: _,
            env: _,
          } => write!(f, "<closure>"),
          Self::Method {
            name,
            params: _,
            body: _,
            env: _,
          } => write!(f, "<m {}>", name),
        }
      }
    }
  }

  mod oo {
    use crate::env::EnvRef;
    use std::fmt::{self, Display};

    #[derive(Clone)]
    pub struct Class {
      pub name: String,
      pub static_methods: EnvRef,
      pub instance_methods: EnvRef,
    }

    impl Class {
      pub fn new(name: String, static_methods: EnvRef, instance_methods: EnvRef) -> Self {
        Self {
          name,
          static_methods,
          instance_methods,
        }
      }
    }

    impl PartialEq for Class {
      fn eq(&self, other: &Self) -> bool {
        self.name == other.name
      }
    }

    impl Display for Class {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
      }
    }

    #[derive(Clone)]
    pub struct Instance {
      pub instance_of: String,
      pub methods: EnvRef,
      pub members: EnvRef,
    }

    impl Instance {
      pub fn new(instance_of: String, methods: EnvRef, members: EnvRef) -> Self {
        Self {
          instance_of,
          methods,
          members,
        }
      }
    }

    impl PartialEq for Instance {
      fn eq(&self, other: &Self) -> bool {
        self.instance_of == other.instance_of && self.members == other.members
      }
    }

    impl Display for Instance {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.instance_of)
      }
    }
  }
}

mod env {
  use super::types::{Airity, Class, Function, NativeFn, Value};
  use std::cell::RefCell;
  use std::cmp::PartialEq;
  use std::collections::HashMap;
  use std::fmt::{self, Display};
  use std::rc::Rc;

  struct Env {
    scope: HashMap<String, Value>,
    enclosing: Option<EnvRef>,
  }

  impl Default for Env {
    fn default() -> Self {
      Self {
        scope: HashMap::new(),
        enclosing: None,
      }
    }
  }

  impl Env {
    fn new(enclosing: EnvRef) -> Env {
      Self {
        scope: HashMap::new(),
        enclosing: Some(enclosing),
      }
    }

    fn define(&mut self, name: &str, value: Value) -> bool {
      self.scope.insert(String::from(name), value).is_some()
    }

    fn lookup(&self, name: &str) -> Option<Value> {
      if let Some(v) = self.scope.get(name) {
        Some(v.clone())
      } else if let Some(enc) = &self.enclosing {
        enc.env.borrow().lookup(name)
      } else {
        None
      }
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
      if self.scope.contains_key(&name) {
        self.scope.insert(name, value);
        Ok(())
      } else if let Some(enc) = &mut self.enclosing {
        enc.env.borrow_mut().assign(name, value)
      } else {
        Err(format!("assignment of undefined variable '{}'", name))
      }
    }
  }

  impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
      self.scope == other.scope
    }
  }

  #[derive(Clone)]
  pub struct EnvRef {
    env: Rc<RefCell<Env>>,
  }

  impl Default for EnvRef {
    fn default() -> Self {
      Self {
        env: Rc::new(RefCell::new(Env::default())),
      }
    }
  }

  impl EnvRef {
    pub fn new_with_enclosing(enclosing: EnvRef) -> Self {
      Self {
        env: Rc::new(RefCell::new(Env::new(enclosing))),
      }
    }

    pub fn snapshot(&self) -> Self {
      Self {
        env: Rc::clone(&self.env),
      }
    }

    pub fn define_native(&mut self, name: &str, airity: Airity, func: NativeFn) -> bool {
      self.define(
        name,
        Value::Callee(Function::new_native(String::from(name), airity, func)),
      )
    }

    pub fn define_class(
      &mut self,
      name: &str,
      static_methods: EnvRef,
      instance_methods: EnvRef,
    ) -> bool {
      self.define(
        name,
        Value::Class(Class {
          name: String::from(name),
          static_methods,
          instance_methods,
        }),
      )
    }

    // returns true if variable was already defined
    pub fn define(&mut self, name: &str, value: Value) -> bool {
      self.env.borrow_mut().define(name, value)
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
      self.env.borrow().lookup(name)
    }

    pub fn lookup_at(&self, distance: usize, name: &str) -> Option<Value> {
      if let Some(envref) = self.ancestor(distance) {
        if let Some(v) = envref.env.borrow().scope.get(name) {
          Some(v.clone())
        } else {
          None
        }
      } else {
        None
      }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
      if let Some(v) = self.env.borrow().scope.get(name) {
        Some(v.clone())
      } else {
        None
      }
    }

    pub fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
      self.env.borrow_mut().assign(name, value)
    }

    pub fn assign_at(&mut self, depth: usize, name: String, value: Value) -> Result<(), String> {
      if let Some(envref) = self.ancestor(depth) {
        envref.env.borrow_mut().assign(name, value)
      } else {
        Err(format!("assignment of undefined variable '{}'", name))
      }
    }

    fn ancestor(&self, distance: usize) -> Option<EnvRef> {
      let mut env = Rc::clone(&self.env);
      for _ in 0..distance {
        let mut tmp = None;
        if let Some(enc) = &env.borrow().enclosing {
          tmp = Some(Rc::clone(&enc.env));
        }
        env = tmp?;
      }
      Some(EnvRef { env })
    }

    fn fmt_indent(&self, f: &mut fmt::Formatter<'_>, indents: usize) -> fmt::Result {
      let tabs = "\t".repeat(indents);
      writeln!(f, "{}scope: {} {{", tabs, indents)?;
      for (k, v) in self.env.borrow().scope.iter() {
        writeln!(f, "{}{} = {}", "\t".repeat(indents + 1), k, v)?;
      }
      writeln!(f, "{}}}", tabs)?;
      if let Some(enc) = &self.env.borrow().enclosing {
        enc.fmt_indent(f, indents + 1)?;
      }

      Ok(())
    }
  }

  impl Display for EnvRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      self.fmt_indent(f, 0)
    }
  }

  impl PartialEq for EnvRef {
    fn eq(&self, other: &Self) -> bool {
      self.env == other.env
    }
  }
}
