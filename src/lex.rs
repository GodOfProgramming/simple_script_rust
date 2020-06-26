use crate::types::Value;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::str;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Minus,
  Plus,
  Semicolon,
  Slash,
  BackSlash,
  Asterisk,
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
  Class,
  Else,
  False,
  Fn,
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
  Load,

  // Line Delimiters
  Eof,
}

#[derive(Clone, PartialEq)]
pub struct Token {
  pub token_type: TokenType,
  pub lexeme: String,
  pub literal: Option<Value>,
  pub line: usize,
}

impl Token {
  pub fn new(token_type: TokenType, lexeme: String, literal: Option<Value>, line: usize) -> Token {
    Token {
      token_type,
      lexeme,
      literal,
      line,
    }
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.token_type {
      TokenType::StringLiteral => write!(f, "{:?}({:?})", self.token_type, self.literal),
      TokenType::NumberLiteral => write!(f, "{:?}({:?})", self.token_type, self.literal),
      _ => write!(f, "{}", self.lexeme),
    }
  }
}

impl Debug for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(self, f)
  }
}

fn basic_keywords() -> HashMap<&'static str, TokenType> {
  let mut map = HashMap::new();
  {
    map.insert("and", TokenType::And);
    map.insert("class", TokenType::Class);
    map.insert("else", TokenType::Else);
    map.insert("false", TokenType::False);
    map.insert("for", TokenType::For);
    map.insert("fn", TokenType::Fn);
    map.insert("if", TokenType::If);
    map.insert("nil", TokenType::Nil);
    map.insert("or", TokenType::Or);
    map.insert("print", TokenType::Print);
    map.insert("return", TokenType::Return);
    map.insert("super", TokenType::Super);
    map.insert("this", TokenType::This);
    map.insert("true", TokenType::True);
    map.insert("var", TokenType::Var);
    map.insert("while", TokenType::While);
    map.insert("load", TokenType::Load);
  }
  map
}

#[derive(Debug)]
pub struct LexicalErr {
  pub file: String,
  pub line: usize,
  pub msg: String,
}

impl Display for LexicalErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} ({}): {}", self.file, self.line, self.msg)
  }
}

pub struct AnalyzeResult {
  pub tokens: Vec<Token>,
  pub lines: usize,
}

pub fn analyze(filename: &str, src: &str) -> Result<AnalyzeResult, LexicalErr> {
  enum TokenResult {
    Valid(TokenType),
    Skip,
    Error { msg: String, line: usize },
  };

  let keywords = basic_keywords();

  let mut tokens = Vec::new();
  let mut line = 0;
  let mut current_pos = 0usize;

  let bytes = src.as_bytes();
  let len = bytes.len();
  while current_pos < len {
    let start_pos = current_pos;
    let c = bytes[current_pos] as char;
    let token = match c {
      '(' => TokenResult::Valid(TokenType::LeftParen),
      ')' => TokenResult::Valid(TokenType::RightParen),
      '{' => TokenResult::Valid(TokenType::LeftBrace),
      '}' => TokenResult::Valid(TokenType::RightBrace),
      ',' => TokenResult::Valid(TokenType::Comma),
      '-' => TokenResult::Valid(TokenType::Minus),
      '+' => TokenResult::Valid(TokenType::Plus),
      ';' => TokenResult::Valid(TokenType::Semicolon),
      '*' => TokenResult::Valid(TokenType::Asterisk),
      '?' => TokenResult::Valid(TokenType::Conditional),
      ':' => TokenResult::Valid(TokenType::Colon),
      '|' => TokenResult::Valid(TokenType::Pipe),
      '.' => {
        if next_is(&bytes, current_pos, '.') {
          current_pos += 1;
          TokenResult::Valid(TokenType::Range)
        } else {
          TokenResult::Valid(TokenType::Dot)
        }
      }
      '!' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenType::ExEq)
        } else {
          TokenResult::Valid(TokenType::Exclamation)
        }
      }
      '=' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenType::EqEq)
        } else {
          TokenResult::Valid(TokenType::Equal)
        }
      }
      '<' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenType::LessEq)
        } else {
          TokenResult::Valid(TokenType::LessThan)
        }
      }
      '>' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenType::GreaterEq)
        } else {
          TokenResult::Valid(TokenType::GreaterThan)
        }
      }
      '/' => {
        // TODO clean this up/make more efficient
        if next_is(&bytes, current_pos, '/') {
          current_pos += 1;
          while let Some(next) = peek(&bytes, current_pos) {
            if next == '\n' {
              break;
            } else {
              current_pos += 1;
            }
          }
          TokenResult::Skip
        } else {
          TokenResult::Valid(TokenType::Slash)
        }
      }
      '"' => {
        // TODO clean this up/make more efficient
        if let Err(line) = loop {
          match peek(&bytes, current_pos) {
            Some(next) => {
              if next != '"' {
                if next == '\n' {
                  line += 1;
                }
              } else {
                break Ok(());
              }
            }
            None => break Err(line),
          }
          current_pos += 1;
        } {
          return Err(LexicalErr {
            file: String::from(filename),
            msg: String::from(r#"missing closing " for string"#),
            line,
          });
        }

        current_pos += 1;

        TokenResult::Valid(TokenType::StringLiteral)
      }
      '\n' => {
        line += 1;
        TokenResult::Skip
      }
      ' ' | '\r' | '\t' => TokenResult::Skip,
      c => {
        if is_digit(c) {
          let mut dot_found = false;
          while let Some(next) = peek(&bytes, current_pos) {
            if is_digit(next) {
              current_pos += 1;
            } else if next == '.' && !dot_found {
              if let Some(next_next) = peek(&bytes, current_pos + 1) {
                if is_digit(next_next) {
                  current_pos += 2;
                  dot_found = true;
                } else {
                  break;
                }
              } else {
                break;
              }
            } else {
              break;
            }
          }

          TokenResult::Valid(TokenType::NumberLiteral)
        } else if is_alpha(c) {
          while let Some(next) = peek(&bytes, current_pos) {
            if is_alphanumeric(next) {
              current_pos += 1;
            } else {
              break;
            }
          }

          match str::from_utf8(&bytes[start_pos..current_pos + 1]) {
            Ok(string) => match keywords.get(string) {
              Some(token) => TokenResult::Valid(token.clone()),
              None => TokenResult::Valid(TokenType::Identifier),
            },
            Err(err) => TokenResult::Error {
              msg: format!("{}", err),
              line,
            },
          }
        } else {
          TokenResult::Error {
            msg: format!("invalid character '{}'", c),
            line,
          }
        }
      }
    };

    current_pos += 1;

    if let TokenResult::Valid(token_type) = token {
      match create_token(&bytes, start_pos, current_pos, token_type) {
        Ok(info) => tokens.push(Token::new(info.token_type, info.lexeme, info.literal, line)),
        Err(err) => {
          return Err(LexicalErr {
            file: String::from(filename),
            msg: err,
            line,
          });
        }
      }
    } else if let TokenResult::Error { msg, line } = token {
      return Err(LexicalErr {
        file: String::from(filename),
        msg,
        line,
      });
    }
  }

  tokens.push(Token::new(TokenType::Eof, String::from("EOF"), None, line));

  Ok(AnalyzeResult {
    tokens: tokens,
    lines: line,
  })
}

fn peek(buff: &[u8], current_pos: usize) -> Option<char> {
  if current_pos + 1 >= buff.len() {
    None
  } else {
    Some(buff[current_pos + 1] as char)
  }
}

struct TokenInfo {
  token_type: TokenType,
  lexeme: String,
  literal: Option<Value>,
}

fn create_token(
  buff: &[u8],
  start: usize,
  end: usize,
  token_type: TokenType,
) -> Result<TokenInfo, String> {
  let lexeme = match str::from_utf8(&buff[start..end]) {
    Ok(string) => string,
    Err(err) => return Err(format!("{}", err)),
  };

  let lexeme = String::from(lexeme);

  let literal = match token_type {
    TokenType::StringLiteral => Some(Value::Str(String::from(&lexeme[1..lexeme.len() - 1]))),
    TokenType::NumberLiteral => match lexeme.parse() {
      Ok(n) => Some(Value::Num(n)),
      Err(err) => return Err(format!("{}", err)),
    },
    TokenType::True => Some(Value::Bool(true)),
    TokenType::False => Some(Value::Bool(false)),
    TokenType::Nil => Some(Value::Nil),
    _ => None,
  };

  Ok(TokenInfo {
    token_type,
    lexeme,
    literal,
  })
}

fn is_digit(c: char) -> bool {
  c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
  is_digit(c) || is_alpha(c)
}

fn next_is(bytes: &[u8], curr_pos: usize, test: char) -> bool {
  if let Some(next) = peek(&bytes, curr_pos) {
    if next == test {
      return true;
    }
  }

  false
}

#[cfg(test)]
mod tests {
  use super::*;

  const GOOD_SRC: &'static str = r#"var var_1 = "some value";"#;

  #[test]
  fn lexer_analyze_with_no_error_basic() {
    let result = analyze("test", GOOD_SRC);

    let expected_tokens = vec![
      Token::new(TokenType::Var, String::from("var"), None, 0),
      Token::new(TokenType::Identifier, String::from("var_1"), None, 0),
      Token::new(TokenType::Equal, String::from("="), None, 0),
      Token::new(
        TokenType::StringLiteral,
        String::from(r#""some value""#),
        Some(Value::Str(String::from("some value"))),
        0,
      ),
      Token::new(TokenType::Semicolon, String::from(";"), None, 0),
      Token::new(TokenType::Eof, String::from("EOF"), None, 0),
    ];

    match result {
      Ok(res) => {
        assert_eq!(res.lines, 0);
        assert_eq!(res.tokens, expected_tokens);
      }
      Err(_) => assert!(false),
    }
  }
}
