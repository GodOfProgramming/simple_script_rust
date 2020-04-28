use regex::Regex;
use std::io::{self, Read, Write};

const STATEMENT_REGEX: &str = r";";
const TOKEN_REGEX: &str = r"([_a-zA-Z][_a-zA-Z0-9]*)|([0-9]*[0-9.][0-9]*)|([:=+*\/;.\{\}\(\)\-])";

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

  pub fn run_string(&self, line: String) {
    let statements = split_statements(line);

    for statement in statements.iter() {
      let tokens = tokenize_statement(String::from(statement));
    }
  }

  pub fn run_script(&self, script: String) {
    for line in script.lines() {
      self.run_string(String::from(line));
    }
  }

  pub fn interactive_mode(&mut self) -> io::Result<()> {
    while self.is_active {
      self.read_from_stdin()?;
    }

    Ok(())
  }
}

fn tokenize_statement(statement: String) -> Vec<Token> {
  let delim = Regex::new(TOKEN_REGEX).unwrap();
  delim.split(&statement).map(|t| Token::new(String::from(t))).collect()
}

fn split_statements(line: String) -> Vec<String> {
  let delim = Regex::new(STATEMENT_REGEX).unwrap();
  delim.split(&line).map(|s| String::from(s)).collect()
}

enum TokenType {
  Integer(i64),
  Float(f64),
  Str(String),
}

struct Token {}

impl Token {
  fn new(token: String) -> Token {
    Token {}
  }
}
