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

  // Line Delimiters
  NewLine,
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
    let mut start_pos = 0usize;
    let mut current_pos = 0usize;

    let bytes = src.as_bytes();
    let mut it = bytes.iter();
    while let Some(byte) = it.next() {
      let token = match *byte as char {
        '(' => Ok((TokenType::LeftParen, "(")),
        ')' => Ok((TokenType::RightParen, ")")),
        '{' => Ok((TokenType::LeftBrace, "{")),
        '}' => Ok((TokenType::RightBrace, "}")),
        ',' => Ok((TokenType::Comma, ",")),
        '.' => Ok((TokenType::Dot, ".")),
        '-' => Ok((TokenType::Minus, "-")),
        '+' => Ok((TokenType::Plus, "+")),
        ';' => Ok((TokenType::Semicolon, ";")),
        '*' => Ok((TokenType::Asterisk, "*")),
        '!' => {
          let mut peek = it.clone();
          match peek.next() {
            Some(next) => match *next as char {
              '=' => {
                it.next();
                Ok((TokenType::ExEq, "!="))
              }
              _ => Ok((TokenType::Equal, "=")),
            },
            None => Ok((TokenType::Equal, "=")),
          }
        }
        '\n' => {
          line += 1;
          Ok((TokenType::NewLine, "\n"))
        }
        _ => Err(line),
      }?;

      tokens.push(Token::new(token.0, Some(String::from(token.1)), line));
    }

    tokens.push(Token::new(TokenType::Eof, None, line));

    Ok(tokens)
  }
}
