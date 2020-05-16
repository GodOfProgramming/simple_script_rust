use std::fmt::{self, Display};

#[derive(Debug)]
pub enum TokenType {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Minus,
  Plus,
  Semicolon,
  Slash,
  Asterisk,

  // One or two character tokens.
  Exclamation,
  ExEq,
  Equal,
  EqEq,
  GreaterThan,
  GreaterEq,
  LessThan,
  LessEq,

  // Literals.
  Identifier,
  StringLiteral(String),
  NumberLiteral(f64),

  // Keywords.
  And,
  Class,
  Else,
  False,
  Fun,
  For,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Var,
  While,

  Eof,
}

pub struct Token {
  token_type: TokenType,
  lexeme: Option<String>,
  line: usize,
}

impl Token {
  pub fn new(token_type: TokenType, lexeme: Option<String>, line: usize) -> Token {
    Token {
      token_type,
      lexeme,
      line,
    }
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.token_type {
      TokenType::StringLiteral(string) => write!(f, "{:?}({})", self.token_type, string),
      TokenType::NumberLiteral(number) => write!(f, "{:?}({})", self.token_type, number),
      other => write!(f, "{:?}", other),
    }
  }
}

pub enum AnalyzeResult {
  Ok,
  CheckNext,
  Err,
}

pub struct Lexer;

impl Lexer {
  pub fn new() -> Lexer {
    Lexer {}
  }

  pub fn analyze(starting_line: usize, src: &str) -> Result<Vec<Token>, usize> {
    let mut tokens = Vec::new();
    let mut line = starting_line;
    let mut start_pos = 0;
    let mut current_pos = 0;

    let type_and_lexeme = |chr: char| -> Result<(TokenType, String), ()> {
      match chr {
        '(' => Ok((TokenType::LeftParen, format!("{}", chr))),
        ')' => Ok((TokenType::RightParen, format!("{}", chr))),
        '{' => Ok((TokenType::LeftBrace, format!("{}", chr))),
        '}' => Ok((TokenType::RightBrace, format!("{}", chr))),
        ',' => Ok((TokenType::Comma, format!("{}", chr))),
        '.' => Ok((TokenType::Dot, format!("{}", chr))),
        '-' => Ok((TokenType::Minus, format!("{}", chr))),
        '+' => Ok((TokenType::Plus, format!("{}", chr))),
        ';' => Ok((TokenType::Semicolon, format!("{}", chr))),
        '*' => Ok((TokenType::Asterisk, format!("{}", chr))),
        _ => Err(()),
      }
    };

    let c = src.chars();
    for chr in src.chars() {
      match type_and_lexeme(chr) {
        Ok(t) => tokens.push(Token::new(t.0, Some(t.1), line)),
        Err(_) => return Err(line),
      }
    }

    tokens.push(Token::new(TokenType::Eof, None, line));

    Ok(tokens)
  }
}
