use crate::ScriptError;
use crate::Value;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt::{self, Debug, Display};
use std::str;

#[derive(Debug, Clone, PartialEq)]
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
  Identifier(String),
  StringLiteral(String),
  NumberLiteral(f64),

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
  Load,
  Loadr,
  Nil,
  Number,
  Or,
  Print,
  Return,
  String,
  True,
  While,

  // Line Delimiters
  Eof,
}

#[derive(Clone, PartialEq)]
pub struct Token {
  pub kind: TokenKind,
  pub file_id: usize,
  pub line: usize,
}

impl Token {
  pub fn new(kind: TokenKind, file_id: usize, line: usize) -> Token {
    Token {
      kind,
      file_id,
      line,
    }
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      TokenKind::StringLiteral(s) => write!(f, "{}", s),
      TokenKind::NumberLiteral(n) => write!(f, "{}", n),
      TokenKind::Identifier(i) => write!(f, "{}", i),
      _ => write!(f, "{:?}", self.kind),
    }
  }
}

impl Debug for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(self, f)
  }
}

fn basic_keywords() -> HashMap<&'static str, TokenKind> {
  let mut map = HashMap::new();
  {
    map.insert("and", TokenKind::And);
    map.insert("bool", TokenKind::Bool);
    map.insert("class", TokenKind::Class);
    map.insert("else", TokenKind::Else);
    map.insert("error", TokenKind::Error);
    map.insert("false", TokenKind::False);
    map.insert("fn", TokenKind::Fn);
    map.insert("for", TokenKind::For);
    map.insert("if", TokenKind::If);
    map.insert("is", TokenKind::Is);
    map.insert("let", TokenKind::Let);
    map.insert("list", TokenKind::List);
    map.insert("load", TokenKind::Load);
    map.insert("loadr", TokenKind::Loadr);
    map.insert("nil", TokenKind::Nil);
    map.insert("number", TokenKind::Number);
    map.insert("or", TokenKind::Or);
    map.insert("print", TokenKind::Print);
    map.insert("return", TokenKind::Return);
    map.insert("string", TokenKind::String);
    map.insert("true", TokenKind::True);
    map.insert("while", TokenKind::While);
  }
  map
}

pub struct AnalyzeResult {
  pub tokens: Vec<Token>,
  pub lines_analyzed: usize,
}

pub fn analyze(file_id: usize, src: &str) -> Result<AnalyzeResult, ScriptError> {
  enum TokenResult {
    Valid(TokenKind),
    Skip,
  };

  let keywords = basic_keywords();

  let mut tokens = Vec::new();
  let mut line = 1;
  let mut current_pos = 0usize;

  let bytes = src.as_bytes();
  let len = bytes.len();
  while current_pos < len {
    let start_pos = current_pos;
    let c = bytes[current_pos] as char;
    let token = match c {
      '(' => TokenResult::Valid(TokenKind::LeftParen),
      ')' => TokenResult::Valid(TokenKind::RightParen),
      '{' => TokenResult::Valid(TokenKind::LeftBrace),
      '}' => TokenResult::Valid(TokenKind::RightBrace),
      ',' => TokenResult::Valid(TokenKind::Comma),
      '-' => TokenResult::Valid(TokenKind::Minus),
      '+' => TokenResult::Valid(TokenKind::Plus),
      '*' => TokenResult::Valid(TokenKind::Asterisk),
      '/' => TokenResult::Valid(TokenKind::Slash),
      ';' => TokenResult::Valid(TokenKind::Semicolon),
      '?' => TokenResult::Valid(TokenKind::Conditional),
      ':' => TokenResult::Valid(TokenKind::Colon),
      '|' => TokenResult::Valid(TokenKind::Pipe),
      '.' => {
        if next_is(&bytes, current_pos, '.') {
          current_pos += 1;
          TokenResult::Valid(TokenKind::Range)
        } else {
          TokenResult::Valid(TokenKind::Dot)
        }
      }
      '!' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenKind::ExEq)
        } else {
          TokenResult::Valid(TokenKind::Exclamation)
        }
      }
      '=' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenKind::EqEq)
        } else {
          TokenResult::Valid(TokenKind::Equal)
        }
      }
      '<' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenKind::LessEq)
        } else {
          TokenResult::Valid(TokenKind::LessThan)
        }
      }
      '>' => {
        if next_is(&bytes, current_pos, '=') {
          current_pos += 1;
          TokenResult::Valid(TokenKind::GreaterEq)
        } else {
          TokenResult::Valid(TokenKind::GreaterThan)
        }
      }
      '#' => {
        while let Some(next) = peek(&bytes, current_pos) {
          if next == '\n' {
            break;
          } else {
            current_pos += 1;
          }
        }
        TokenResult::Skip
      }
      '"' => {
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
          return Err(ScriptError {
            file_id,
            line,
            msg: String::from("missing closing \" for string"),
          });
        }

        current_pos += 1;

        let lexeme =
          str::from_utf8(&bytes[start_pos + 1..current_pos - 1]).map_err(|err| ScriptError {
            file_id,
            line,
            msg: format!("cannot read string literal: {}", err),
          })?;

        TokenResult::Valid(TokenKind::StringLiteral(String::from(lexeme)))
      }
      '\n' => {
        line += 1;
        TokenResult::Skip
      }
      ' ' | '\r' | '\t' => TokenResult::Skip,
      c if is_digit(c) => {
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

        let lexeme = match str::from_utf8(&bytes[start_pos..current_pos]) {
          Ok(string) => string,
          Err(err) => return Err(format!("{}", err)),
        };

        let num = lexeme.parse().map_err(|err| ScriptError {
          file_id,
          line,
          msg: format!("{}", err),
        })?;

        TokenResult::Valid(TokenKind::NumberLiteral(num))
      }
      c if is_alpha(c) => {
        while let Some(next) = peek(&bytes, current_pos) {
          if is_alphanumeric(next) {
            current_pos += 1;
          } else {
            break;
          }
        }

        match str::from_utf8(&bytes[start_pos..current_pos + 1]) {
          Ok(string) => match keywords.get(string) {
            Some(token) => TokenResult::Valid(*token),
            None => TokenResult::Valid(TokenKind::Identifier(String::from(string))),
          },
          Err(err) => {
            return Err(ScriptError {
              file_id,
              line,
              msg: format!("{}", err),
            });
          }
        }
      }
      c => {
        return Err(ScriptError {
          file_id,
          line,
          msg: format!("invalid character '{}'", c),
        })
      }
    };

    current_pos += 1;

    if let TokenResult::Valid(token_type) = token {
      tokens.push(Token::new(token_type, file_id, line));
    }
  }

  tokens.push(Token::new(TokenKind::Eof, file_id, line));

  Ok(AnalyzeResult {
    tokens,
    lines_analyzed: line - 1, // sub 1 b/c line starts at 1
  })
}

fn peek(buff: &[u8], current_pos: usize) -> Option<char> {
  if current_pos + 1 >= buff.len() {
    None
  } else {
    Some(buff[current_pos + 1] as char)
  }
}

fn is_digit(c: char) -> bool {
  c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '@'
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

  const GOOD_SRC: &str = "let var_1 = 1;";

  #[test]
  fn lexer_analyze_with_no_error_basic() {
    let result = analyze(0, GOOD_SRC);

    let expected_tokens = vec![
      Token::new(TokenKind::Let, 0, 1),
      Token::new(TokenKind::Identifier(String::from("var_1")), 0, 1),
      Token::new(TokenKind::Equal, 0, 1),
      Token::new(TokenKind::NumberLiteral(1.0), 0, 1),
      Token::new(TokenKind::Semicolon, 0, 1),
      Token::new(TokenKind::Eof, 0, 1),
    ];

    match result {
      Ok(res) => {
        assert_eq!(res.lines_analyzed, 0);
        assert_eq!(res.tokens, expected_tokens);
      }
      Err(_) => panic!("failed to pass lexical analysis"),
    }
  }
}
