use super::*;

const ALL_TOKENS: &str = include_str!("all_tokens.ss");

const TOKENS_THAT_IGNORE_WHITESPACE: &str = include_str!("tokens_that_ignore_whitespace.ss");

const SCANNING_ERRORS: &str = include_str!("scanning_errors.ss");

#[test]
fn scanner_scans() {
  let mut scanner = Scanner::new("test", ALL_TOKENS);
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
    Token::Number(PI),
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
  if let Ok((actual, _)) = actual {
    assert_eq!(actual.len(), expected.len());

    for (t0, t1) in actual.iter().zip(expected.iter()) {
      assert_eq!(t0, t1);
    }
  } else {
    panic!("failed to scan");
  }
}

#[test]
fn scanner_ignores_whitespace_when_applicable() {
  let mut scanner = Scanner::new("test", TOKENS_THAT_IGNORE_WHITESPACE);
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
    Token::Identifier(String::from("foo")),
    Token::BangEqual,
    Token::Identifier(String::from("bar")),
    Token::Equal,
    Token::Identifier(String::from("foo")),
    Token::EqualEqual,
    Token::Identifier(String::from("bar")),
    Token::Greater,
    Token::Identifier(String::from("foo")),
    Token::GreaterEqual,
    Token::Identifier(String::from("bar")),
    Token::Less,
    Token::Identifier(String::from("foo")),
    Token::LessEqual,
    Token::Identifier(String::from("bar")),
    Token::Arrow,
    Token::Number(1.0),
    Token::Bang,
    Token::Number(1.0),
    Token::BangEqual,
    Token::Number(1.0),
    Token::Equal,
    Token::Number(1.0),
    Token::EqualEqual,
    Token::Number(1.0),
    Token::Greater,
    Token::Number(1.0),
    Token::GreaterEqual,
    Token::Number(1.0),
    Token::Less,
    Token::Number(1.0),
    Token::LessEqual,
    Token::Number(1.0),
    Token::Arrow,
    Token::String(String::from("str")),
    Token::Bang,
    Token::String(String::from("str")),
    Token::BangEqual,
    Token::String(String::from("str")),
    Token::Equal,
    Token::String(String::from("str")),
    Token::EqualEqual,
    Token::String(String::from("str")),
    Token::Greater,
    Token::String(String::from("str")),
    Token::GreaterEqual,
    Token::String(String::from("str")),
    Token::Less,
    Token::String(String::from("str")),
    Token::LessEqual,
    Token::String(String::from("str")),
    Token::Arrow,
  ];

  if let Ok((actual, _)) = actual {
    assert_eq!(
      actual.len(),
      expected.len(),
      "actual len = {}, expected len = {}",
      actual.len(),
      expected.len(),
    );

    for (a, e) in actual.iter().zip(expected.iter()) {
      assert_eq!(a, e, "actual = {:?}, expected = {:?}", a, e);
    }
  } else {
    panic!("failed to scan");
  }
}

#[test]
fn scans_empty_string() {
  let mut scanner = Scanner::new("test", "\"\"");
  let actual = scanner.scan();
  let expected = vec![Token::String(String::from(""))];

  if let Ok((actual, _)) = actual {
    assert_eq!(
      actual.len(),
      expected.len(),
      "actual len = {}, expected len = {}",
      actual.len(),
      expected.len(),
    );

    for (a, e) in actual.iter().zip(expected.iter()) {
      assert_eq!(a, e, "actual = {:?}, expected = {:?}", a, e);
    }
  } else {
    panic!("failed to scan");
  }
}

#[test]
fn returns_errors_at_right_location_when_detected() {
  let mut scanner = Scanner::new("test", SCANNING_ERRORS);
  let actual = scanner.scan();

  let expected = vec![
    Error {
      msg: String::from("invalid character: '^'"),
      file: String::from("test"),
      line: 6,
      column: 8,
    },
    Error {
      msg: String::from("invalid character: '?'"),
      file: String::from("test"),
      line: 7,
      column: 11,
    },
    Error {
      msg: String::from("invalid character: ':'"),
      file: String::from("test"),
      line: 7,
      column: 15,
    },
  ];

  if let Err(actual) = actual {
    assert_eq!(
      actual.len(),
      expected.len(),
      "actual len = {}, expected len = {}",
      actual.len(),
      expected.len(),
    );

    for (a, e) in actual.iter().zip(expected.iter()) {
      assert_eq!(a, e, "actual = {:?}, expected = {:?}", a, e);
    }
  } else {
    panic!("scanner should not have succeeded");
  }
}
