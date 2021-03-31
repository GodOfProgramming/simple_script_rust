use crate::{
  types::{Env, Value},
  Error, New,
};
use std::{
  f64::consts::PI,
  fmt::{self, Debug, Display},
  str,
};

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, PartialEq, Clone)]
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
  Undef,
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

#[derive(Debug, PartialEq)]
pub struct TokenMeta<'file> {
  pub file: &'file str,
  pub line: usize,
  pub column: usize,
}

pub struct CodeMeta {
  file: String,
  source: String,
  opcode_info: Vec<(usize, usize)>,
}

impl CodeMeta {
  fn new(file: String, source: String) -> Self {
    Self {
      file,
      source,
      opcode_info: Vec::default(),
    }
  }

  fn add(&mut self, line: usize, column: usize) {
    self.opcode_info.push((line, column));
  }

  fn get(&self, offset: usize) -> Option<(String, String, usize, usize)> {
    if let Some((line, column)) = self.opcode_info.get(offset).cloned() {
      if let Some(src) = self.source.lines().nth(line - 1) {
        Some((String::from(src), self.file.clone(), line, column))
      } else {
        None
      }
    } else {
      None
    }
  }
}

pub struct Context {
  instructions: Vec<OpCode>,
  ip: usize,

  env: Env,
  stack: Vec<Value>,
  consts: Vec<Value>,

  meta: CodeMeta,
}

impl Context {
  fn new(meta: CodeMeta) -> Self {
    Self {
      instructions: Vec::default(),
      ip: 0,
      env: Env::default(),
      stack: Vec::default(),
      consts: Vec::default(),
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

  pub fn lookup_global(&self, name: &str) -> Option<Value> {
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

  fn write(&mut self, op: OpCode, line: usize, column: usize) {
    self.instructions.push(op);
    self.meta.add(line, column);
  }

  fn write_const(&mut self, c: Value, line: usize, column: usize) {
    self.write(OpCode::Const(self.consts.len()), line, column);
    self.consts.push(c);
  }

  fn num_instructions(&self) -> usize {
    self.instructions.len()
  }

  fn replace_instruction(&mut self, index: usize, op: OpCode) -> bool {
    if let Some(inst) = self.instructions.get_mut(index) {
      *inst = op;
      true
    } else {
      false
    }
  }

  pub fn reflect_instruction<F: FnOnce(String, String, usize, usize) -> Error>(
    &self,
    f: F,
  ) -> Error {
    if let Some((src, file, line, column)) = self.meta.get(self.ip) {
      f(src, file, line, column)
    } else {
      Error {
        msg: format!("could not fetch info for instruction {:04X}", self.ip),
        file: self.meta.file.clone(),
        line: 0,
        column: 0,
      }
    }
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
    if let Some(curr) = self.meta.get(offset) {
      if offset > 0 {
        if let Some(prev) = self.meta.get(offset - 1) {
          if curr.0 == prev.0 {
            print!("   | ");
          }
        } else {
          print!("?????");
        }
      } else {
        print!("{:#04} ", curr.0);
      }
    } else {
      print!("?????");
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
  file: &'src str,
  raw_src: &'src [u8],
  start_pos: usize,
  pos: usize,
  line: usize,
  column: usize,
  errors: Option<Vec<Error>>,
}

impl<'src> Scanner<'src> {
  fn new(file: &'src str, source: &'src str) -> Self {
    Scanner {
      file,
      raw_src: source.as_bytes(),
      start_pos: 0,
      pos: 0,
      line: 0,
      column: 0,
      errors: None,
    }
  }

  fn scan(&mut self) -> Result<(Vec<Token>, Vec<TokenMeta>), Vec<Error>> {
    let mut tokens = Vec::new();
    let mut meta = Vec::new();

    loop {
      self.skip_whitespace();
      let mut should_advance = true;
      if let Some(c) = self.peek() {
        self.start_pos = self.pos;

        let line = self.line;
        let column = self.column;

        if cfg!(test) {
          println!(
            "pos = {}, line = {}, col = {}",
            self.pos, self.line, self.column,
          );
        }

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
          c if Self::is_digit(c) => {
            should_advance = false;
            self.make_number()
          }
          c if Self::is_alpha(c) => {
            should_advance = false;
            self.make_ident()
          }
          c => {
            self.error(format!("invalid character: '{}'", c));
            Token::Invalid
          }
        };

        if cfg!(test) {
          println!("made token {:?}", token);
        }

        tokens.push(token);
        meta.push(TokenMeta {
          file: self.file,
          line: line + 1,
          column: column + 1,
        });

        if should_advance {
          self.advance();
        }
      } else {
        break;
      }
    }

    if let Some(errs) = self.errors.take() {
      Err(errs)
    } else {
      Ok((tokens, meta))
    }
  }

  fn error(&mut self, msg: String) {
    if self.errors.is_none() {
      self.errors = Some(Vec::new());
    }

    if let Some(errs) = &mut self.errors {
      errs.push(Error {
        msg,
        file: String::from(self.file),
        line: self.line + 1,
        column: self.column + 1,
      });
    }
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
      Err(e) => {
        self.error(format!("{} ('{}')", e, lexeme));
        Token::Invalid
      }
    }
  }

  fn make_string(&mut self) -> Token {
    self.advance(); // skip the first "
    let mut error_detected = false;
    while let Some(c) = self.peek() {
      match c {
        '"' => break,
        '\n' => {
          self.error(String::from("multiline strings are unsupported"));
          error_detected = true;
          self.advance();
          self.line += 1;
          self.column = 0;
        }
        _ => self.advance(),
      }
    }

    if error_detected {
      return Token::Invalid;
    }

    if self.at_end() {
      self.error(String::from("unterminated string"));
      return Token::Invalid;
    }

    match str::from_utf8(&self.raw_src[self.start_pos + 1..self.pos]) {
      Ok(string) => Token::String(String::from(string)),
      Err(e) => {
        self.error(format!("{}", e));
        Token::Invalid
      }
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

  fn create_ident(&mut self) -> Token {
    match str::from_utf8(&self.raw_src[self.start_pos..self.pos]) {
      Ok(string) => Token::Identifier(String::from(string)),
      Err(e) => {
        self.error(format!("{}", e));
        Token::Invalid
      }
    }
  }

  fn check_keywords(&mut self) -> Token {
    let do_at_depth =
      |this: &mut Self, depth, f: &dyn Fn(&mut Self, usize, char) -> Token| -> Token {
        if let Some(c) = this.index_n(this.start_pos + depth) {
          f(this, depth + 1, c)
        } else {
          this.create_ident()
        }
      };

    do_at_depth(self, 0, &|this, d, c0| match c0 {
      'a' => this.check_keyword(d, "nd", Token::And),
      'b' => this.check_keyword(d, "reak", Token::Break),
      'c' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'l' => this.check_keyword(d, "ass", Token::Class),
        'o' => this.check_keyword(d, "nt", Token::Cont),
        _ => this.create_ident(),
      }),
      'e' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'l' => this.check_keyword(d, "se", Token::Else),
        'n' => this.check_keyword(d, "d", Token::End),
        _ => this.create_ident(),
      }),
      'f' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'a' => this.check_keyword(d, "lse", Token::False),
        'n' => this.check_keyword(d, "", Token::Fn),
        'o' => this.check_keyword(d, "r", Token::For),
        _ => this.create_ident(),
      }),
      'i' => this.check_keyword(d, "f", Token::If),
      'l' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'e' => this.check_keyword(d, "t", Token::Let),
        'o' => do_at_depth(this, d, &|this, d, c2| match c2 {
          'a' => this.check_keyword(d, "d", Token::Load),
          'o' => this.check_keyword(d, "p", Token::Loop),
          _ => this.create_ident(),
        }),
        _ => this.create_ident(),
      }),
      'm' => this.check_keyword(d, "atch", Token::Match),
      'n' => this.check_keyword(d, "il", Token::Nil),
      'o' => this.check_keyword(d, "r", Token::Or),
      'p' => this.check_keyword(d, "rint", Token::Print),
      'r' => this.check_keyword(d, "et", Token::Ret),
      't' => this.check_keyword(d, "rue", Token::True),
      'u' => this.check_keyword(d, "ndef", Token::Undef),
      'w' => this.check_keyword(d, "hile", Token::While),
      _ => this.create_ident(),
    })
  }

  fn check_keyword(&mut self, start: usize, rest: &str, checkee: Token) -> Token {
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
          self.advance();
          self.line += 1;
          self.column = 0;
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
    self.column += 1;
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
    ('0'..='9').contains(&c)
  }

  fn is_alpha(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' || c == '@'
  }

  fn is_alphanumeric(c: char) -> bool {
    Self::is_alpha(c) || Self::is_digit(c)
  }
}

#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
  None,
  Assignment, // =
  Or,         // or
  And,        // and
  Equality,   // == !=
  Comparison, // < > <= >=
  Term,       // + -
  Factor,     // / *
  Unary,      // - !
  Call,       // . ()
  Primary,
}

type ParseFn<'file> = fn(&mut Parser<'file>, bool) -> bool;

struct ParseRule<'file> {
  prefix: Option<ParseFn<'file>>,
  infix: Option<ParseFn<'file>>,
  precedence: Precedence,
}

impl<'file> ParseRule<'file> {
  fn new(
    prefix: Option<ParseFn<'file>>,
    infix: Option<ParseFn<'file>>,
    precedence: Precedence,
  ) -> Self {
    Self {
      prefix,
      infix,
      precedence,
    }
  }
}

struct Local {
  name: String,
  depth: usize,
  initialized: bool,
}

#[derive(PartialEq)]
enum LookupKind {
  Local,
  Global,
}

struct Lookup {
  kind: LookupKind,
  index: usize,
}

pub struct Parser<'file> {
  tokens: Vec<Token>,
  meta: Vec<TokenMeta<'file>>,
  index: usize,

  ctx: Option<Context>,
  errors: Option<Vec<Error>>,

  locals: Vec<Local>,
}

impl<'file> Parser<'file> {
  pub fn new(
    tokens: Vec<Token>,
    meta: Vec<TokenMeta<'file>>,
    file: String,
    source: String,
  ) -> Self {
    let code_meta = CodeMeta::new(file, source);
    let ctx = Context::new(code_meta);
    Self {
      tokens,
      meta,
      index: 0,
      ctx: Some(ctx),
      errors: None,
      locals: Vec::new(),
    }
  }

  fn parse(&mut self) -> Result<Context, Vec<Error>> {
    while let Some(current) = self.current() {
      self.declaration(current);
    }

    if let Some(errors) = self.errors.take() {
      Err(errors)
    } else if let Some(ctx) = self.ctx.take() {
      Ok(ctx)
    } else {
      panic!("this should not ever happen");
    }
  }

  fn error(&mut self, pos: usize, msg: String) {
    if self.errors.is_none() {
      self.errors = Some(Vec::new());
    }

    let meta = self.meta.get(pos);

    if let Some(errs) = &mut self.errors {
      if let Some(meta) = meta {
        errs.push(Error {
          msg,
          file: String::from(meta.file),
          line: meta.line,
          column: meta.column,
        });
      }
    }

    self.sync();
  }

  fn current(&self) -> Option<Token> {
    self.tokens.get(self.index).cloned()
  }

  fn previous(&self) -> Option<Token> {
    if self.index > 0 {
      self.tokens.get(self.index - 1).cloned()
    } else {
      None
    }
  }

  fn advance(&mut self) {
    self.index += 1;
  }

  fn advance_if_matches(&mut self, token: Token) -> bool {
    if let Some(curr) = self.current() {
      if curr == token {
        self.advance();
        return true;
      }
    }
    false
  }

  fn consume(&mut self, expected: Token, err: String) -> bool {
    if let Some(curr) = self.current() {
      if curr == expected {
        self.advance();
        true
      } else {
        self.error(self.index, err);
        false
      }
    } else {
      self.error(
        self.index - 1,
        format!("tried to lookup a token in an invalid index: {}", err),
      );
      false
    }
  }

  fn emit(&mut self, pos: usize, op: OpCode) {
    if let Some(meta) = self.meta.get(pos) {
      if let Some(ctx) = &mut self.ctx {
        ctx.write(op, meta.line, meta.column);
      } else {
        panic!("should never happen");
      }
    } else {
      panic!("tried to acquire metainfo about a token that does not exist");
    }
  }

  fn emit_const(&mut self, pos: usize, c: Value) {
    if let Some(meta) = self.meta.get(pos) {
      if let Some(ctx) = &mut self.ctx {
        ctx.write_const(c, meta.line, meta.column);
      } else {
        panic!("should never happen");
      }
    } else {
      panic!("tried to acquire metainfo about a token that does not exist");
    }
  }

  fn emit_jump(&mut self, pos: usize, op: OpCode) -> usize {
    if let Some(ctx) = &self.ctx {
      let offset = ctx.num_instructions();
      self.emit(pos, op);
      offset
    } else {
      panic!("this should not be possible");
    }
  }

  fn patch_jump<F: FnOnce(&mut Context, usize, usize) -> bool>(
    &mut self,
    jmp_instr: usize,
    f: F,
  ) -> bool {
    if let Some(ctx) = &mut self.ctx {
      let offset = ctx.num_instructions() - jmp_instr;
      f(ctx, jmp_instr, offset)
    } else {
      panic!("this should not be possible");
    }
  }

  fn declaration(&mut self, token: Token) {
    match token {
      // Token::Break => {
      //   self.advance();
      //   self.break_stmt();
      // }
      // Token::Cont => {
      //   self.advance();
      //   self.cont_stmt();
      // }
      // Token::End => {
      //   self.advance();
      //   self.end_stmt();
      // }
      // Token::Fn => {
      //   self.advance();
      //   self.fn_stmt();
      // }
      // Token::For => {
      //   self.advance();
      //   self.For_stmt();
      // }
      // Token::If => {
      //   self.advance();
      //   self.if_stmt();
      // }
      // Token::LeftBrace => {
      //   self.advance();
      //   self.block_stmt();
      // }
      // Token::Let => {
      //   self.advance();
      //   self.let_stmt();
      // }
      // Token::Load => {
      //   self.advance();
      //   self.load_stmt();
      // }
      // Token::Loop => {
      //   self.advance();
      //   self.loop_stmt();
      // }
      // Token::Match => {
      //   self.advance();
      //   self.match_stmt();
      // }
      Token::Print => {
        self.advance();
        self.print_stmt();
      }
      // Token::Ret => {
      //   self.advance();
      //   self.ret_stmt();
      // }
      // Token::While => {
      //   self.advance();
      //   self.while_stmt();
      // }
      _ => (), // self.expression_stmt(),
    }
  }

  fn print_stmt(&mut self) {
    let pos = self.index - 1;
    if !self.expression() {
      return;
    }
    if !self.consume(Token::Semicolon, String::from("expected ';' after value")) {
      return;
    }
    self.emit(pos, OpCode::Print);
  }

  fn expression(&mut self) -> bool {
    self.parse_precedence(Precedence::Assignment)
  }

  fn rule_for(token: &Token) -> ParseRule<'file> {
    match token {
      Token::Invalid => ParseRule::new(None, None, Precedence::None),
      Token::LeftParen => ParseRule::new(
        Some(Parser::grouping_expr),
        Some(Parser::call_expr),
        Precedence::Call,
      ),
      Token::RightParen => ParseRule::new(None, None, Precedence::None),
      Token::LeftBrace => ParseRule::new(None, None, Precedence::None),
      Token::RightBrace => ParseRule::new(None, None, Precedence::None),
      Token::Comma => ParseRule::new(None, None, Precedence::None),
      Token::Dot => ParseRule::new(None, None, Precedence::None),
      Token::Semicolon => ParseRule::new(None, None, Precedence::None),
      Token::Plus => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Term),
      Token::Minus => ParseRule::new(
        Some(Parser::unary_expr),
        Some(Parser::binary_expr),
        Precedence::Term,
      ),
      Token::Asterisk => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Factor),
      Token::Slash => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Factor),
      Token::Modulus => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Factor),
      Token::Bang => ParseRule::new(Some(Parser::unary_expr), None, Precedence::None),
      Token::BangEqual => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Equality),
      Token::Equal => ParseRule::new(None, None, Precedence::None),
      Token::EqualEqual => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Equality),
      Token::Greater => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison),
      Token::GreaterEqual => {
        ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison)
      }
      Token::Less => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison),
      Token::LessEqual => ParseRule::new(None, Some(Parser::binary_expr), Precedence::Comparison),
      Token::Arrow => ParseRule::new(None, None, Precedence::None),
      Token::Identifier(_) => ParseRule::new(Some(Parser::make_variable), None, Precedence::None),
      Token::String(_) => ParseRule::new(Some(Parser::make_string), None, Precedence::None),
      Token::Number(_) => ParseRule::new(Some(Parser::make_number), None, Precedence::None),
      Token::And => ParseRule::new(None, Some(Parser::and_expr), Precedence::And),
      Token::Break => ParseRule::new(None, None, Precedence::None),
      Token::Class => ParseRule::new(None, None, Precedence::None),
      Token::Cont => ParseRule::new(None, None, Precedence::None),
      Token::Else => ParseRule::new(None, None, Precedence::None),
      Token::False => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::For => ParseRule::new(None, None, Precedence::None),
      Token::Fn => ParseRule::new(None, None, Precedence::None),
      Token::If => ParseRule::new(None, None, Precedence::None),
      Token::Let => ParseRule::new(None, None, Precedence::None),
      Token::Load => ParseRule::new(None, None, Precedence::None),
      Token::Loop => ParseRule::new(None, None, Precedence::None),
      Token::Match => ParseRule::new(None, None, Precedence::None),
      Token::Nil => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::Or => ParseRule::new(None, Some(Parser::or_expr), Precedence::Or),
      Token::Print => ParseRule::new(None, None, Precedence::None),
      Token::Ret => ParseRule::new(None, None, Precedence::None),
      Token::True => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::Undef => ParseRule::new(Some(Parser::literal_expr), None, Precedence::None),
      Token::While => ParseRule::new(None, None, Precedence::None),
      Token::End => ParseRule::new(None, None, Precedence::None),
    }
  }

  fn parse_precedence(&mut self, precedence: Precedence) -> bool {
    if let Some(prev) = self.current() {
      self.advance();

      let rule = Parser::rule_for(&prev);

      let can_assign = precedence <= Precedence::Assignment;

      if let Some(prefix) = rule.prefix {
        if !prefix(self, can_assign) {
          return false;
        }
      } else {
        self.error(self.index, String::from("expected an expression"));
        return false;
      }

      while let Some(curr) = self.current() {
        if precedence <= Parser::rule_for(&curr).precedence {
          self.advance();
          if let Some(prev) = self.previous() {
            if let Some(infix) = Parser::rule_for(&prev).infix {
              if !infix(self, can_assign) {
                return false;
              }
            } else {
              todo!("error condition here")
            }
          } else {
            todo!("should never error but just for sanity sake")
          }
        } else {
          break;
        }
      }

      if can_assign && self.advance_if_matches(Token::Equal) {
        self.error(self.index, String::from("invalid assignment target"));
        false
      } else {
        true
      }
    } else {
      todo!("error condition here");
    }
  }

  fn make_number(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      if let Token::Number(n) = prev {
        self.emit_const(pos, Value::new(n));
        true
      } else {
        self.error(pos, format!("expected number, found {}", prev));
        false
      }
    } else {
      todo!("error here");
    }
  }

  fn make_string(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      if let Token::String(s) = prev {
        self.emit_const(pos, Value::new(s));
        true
      } else {
        self.error(pos, format!("expected string, found {}", prev));
        false
      }
    } else {
      todo!("error here");
    }
  }

  fn make_variable(&mut self, can_assign: bool) -> bool {
    if let Some(prev) = self.previous() {
      self.named_variable(prev, self.index - 1, can_assign)
    } else {
      todo!("error here");
    }
  }

  fn grouping_expr(&mut self, _: bool) -> bool {
    if !self.expression() {
      return false;
    }
    self.consume(Token::RightParen, String::from(""))
  }

  fn call_expr(&mut self, _: bool) -> bool {
    unimplemented!();
  }

  fn literal_expr(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      match prev {
        Token::Nil => {
          self.emit(self.index - 1, OpCode::Nil);
          true
        }
        Token::True => {
          self.emit(self.index - 1, OpCode::True);
          true
        }
        Token::False => {
          self.emit(self.index - 1, OpCode::False);
          true
        }
        _ => {
          self.error(
            self.index - 1,
            String::from("reaching this means something is very screwed up"),
          );
          false
        }
      }
    } else {
      todo!("error here");
    }
  }

  fn unary_expr(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      if !self.parse_precedence(Precedence::Unary) {
        return false;
      }

      match prev {
        Token::Bang => self.emit(pos, OpCode::Not),
        Token::Minus => self.emit(pos, OpCode::Negate),
        _ => {
          self.error(pos, String::from("invalid unary operator"));
          return false;
        }
      }

      true
    } else {
      todo!("error here");
    }
  }

  fn binary_expr(&mut self, _: bool) -> bool {
    if let Some(prev) = self.previous() {
      let pos = self.index - 1;
      let rule = Self::rule_for(&prev);
      if !self.parse_precedence(rule.precedence) {
        return false;
      }

      match prev {
        Token::EqualEqual => self.emit(pos, OpCode::Equal),
        Token::BangEqual => self.emit(pos, OpCode::NotEqual),
        Token::Greater => self.emit(pos, OpCode::Greater),
        Token::GreaterEqual => self.emit(pos, OpCode::GreaterEqual),
        Token::Less => self.emit(pos, OpCode::Less),
        Token::LessEqual => self.emit(pos, OpCode::LessEqual),
        Token::Plus => self.emit(pos, OpCode::Add),
        Token::Minus => self.emit(pos, OpCode::Sub),
        Token::Asterisk => self.emit(pos, OpCode::Mul),
        Token::Slash => self.emit(pos, OpCode::Div),
        Token::Modulus => self.emit(pos, OpCode::Mod),
        _ => {
          self.error(pos, String::from("invalid binary operator"));
          return false;
        }
      }

      true
    } else {
      todo!("error here");
    }
  }

  fn and_expr(&mut self, _: bool) -> bool {
    let jmp_pos = self.emit_jump(self.index, OpCode::NoOp);
    if !self.parse_precedence(Precedence::And) {
      return false;
    }
    self.patch_jump(jmp_pos, |ctx, jmp_instr, offset| {
      ctx.replace_instruction(jmp_instr, OpCode::And(offset))
    })
  }

  fn or_expr(&mut self, _: bool) -> bool {
    let jmp_pos = self.emit_jump(self.index, OpCode::NoOp);
    if !self.parse_precedence(Precedence::Or) {
      return false;
    }
    self.patch_jump(jmp_pos, |ctx, jmp_instr, offset| {
      ctx.replace_instruction(jmp_instr, OpCode::Or(offset))
    })
  }

  fn named_variable(&mut self, token: Token, pos: usize, can_assign: bool) -> bool {
    if let Some(lookup) = self.resolve_local(&token, pos) {
      let get: OpCode;
      let set: OpCode;
      if lookup.kind == LookupKind::Local {
        get = OpCode::LookupLocal(lookup.index);
        set = OpCode::AssignLocal(lookup.index);
      } else if let Token::Identifier(name) = token {
        get = OpCode::LookupGlobal(name.clone());
        set = OpCode::AssignGlobal(name);
      } else {
        self.error(pos, format!("unable to parse lookup of var '{}'", token));
        return false;
      }
      if can_assign && self.advance_if_matches(Token::Equal) {
        self.expression();
        self.emit(pos, set);
      } else {
        self.emit(pos, get);
      }
      true
    } else {
      false
    }
  }

  fn resolve_local(&mut self, token: &Token, pos: usize) -> Option<Lookup> {
    let mut index = self.locals.len() - 1;

    for local in self.locals.iter().rev() {
      if let Token::Identifier(name) = token {
        if *name == local.name {
          if !local.initialized {
            self.error(
              pos,
              String::from("can't read variable in it's own initializer"),
            );
            return None;
          } else {
            return Some(Lookup {
              index,
              kind: LookupKind::Local,
            });
          }
        }
      } else {
        todo!("error here");
      }
      index -= 1;
    }

    Some(Lookup {
      index: 0,
      kind: LookupKind::Global,
    })
  }

  fn sync(&mut self) {
    while let Some(curr) = self.current() {
      if let Some(prev) = self.previous() {
        if prev == Token::Semicolon {
          return;
        }
      }

      if matches!(
        curr,
        Token::Class
          | Token::Fn
          | Token::Let
          | Token::For
          | Token::If
          | Token::While
          | Token::Print
          | Token::Ret
          | Token::Match
          | Token::Loop
          | Token::End
      ) {
        return;
      }

      self.advance();
    }
  }
}

pub struct Compiler;

impl Compiler {
  pub fn compile(&self, file: &str, source: &str) -> Result<Context, Vec<Error>> {
    let mut scanner = Scanner::new(file, source);

    let (tokens, meta) = scanner
      .scan()
      .map_err(|errs| self.reformat_errors(source, errs))?;

    let mut parser = Parser::new(tokens, meta, String::from(file), String::from(source));

    parser.parse()
  }

  fn reformat_errors(&self, source: &str, errs: Vec<Error>) -> Vec<Error> {
    errs
      .into_iter()
      .map(|mut e| {
        if let Some(src) = source.lines().nth(e.line - 1) {
          e.format_with_src_line(String::from(src));
        }
        e
      })
      .collect()
  }
}

#[cfg(test)]
mod test;