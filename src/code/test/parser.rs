use super::*;

const EXAMPLE_SCRIPT: &str = include_str!("parser_example_script.ss");
const SYNC_TEST_SCRIPT: &str = include_str!("sync_test_script.ss");

#[cfg(test)]
fn do_with_parser<F: FnOnce(Parser)>(script: &str, f: F) {
  let mut scanner = Scanner::new("test", script);

  let (tokens, meta) = scanner.scan().unwrap();

  let parser = Parser::new(tokens, meta, String::from("test"), String::from(script));

  f(parser);
}

#[cfg(test)]
fn do_with_ctx<F: FnOnce(Context)>(mut parser: Parser, f: F) {
  if let Some(ctx) = parser.ctx.take() {
    f(ctx);
  } else {
    panic!("this should never happen");
  }
}

#[test]
fn current_returns_the_current_token() {
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    let expected = Token::RightParen;
    parser.index = 3; // increment the index
    let actual = parser.current().unwrap();
    assert_eq!(actual, expected);
  });
}

#[test]
fn previous_returns_the_last_token() {
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    let expected = Token::LeftParen;
    parser.index = 3; // increment the index
    let actual = parser.previous().unwrap();
    assert_eq!(actual, expected);
  });
}

#[test]
fn advance_increments_the_index_by_1() {
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    let curr = parser.index;
    parser.advance();
    assert_eq!(curr + 1, parser.index);
  });
}

#[test]
fn advance_if_matches_advances_only_if_theres_a_match() {
  // valid
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    parser.index = 3;
    assert!(parser.advance_if_matches(Token::RightParen));
    assert_eq!(parser.index, 4);
  });

  // does not match
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    parser.index = 3;
    assert!(!parser.advance_if_matches(Token::LeftParen));
    assert_eq!(parser.index, 3);
  });

  // index too large
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    parser.index = 999;
    assert!(!parser.advance_if_matches(Token::RightParen));
    assert_eq!(parser.index, 999);
  });
}

#[test]
fn consume_advances_only_if_expected_is_accurate() {
  // valid
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    assert!(parser.consume(Token::Fn, String::from("unused")));
    assert_eq!(parser.index, 1);
  });

  // valid
  do_with_parser(EXAMPLE_SCRIPT, |mut parser| {
    assert!(!parser.consume(Token::LeftParen, String::from("used")));
    assert_eq!(parser.index, 0);
    let errors = parser.errors.unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(
      errors[0],
      Error {
        msg: String::from("used"),
        file: String::from("test"),
        line: 1,
        column: 1,
      }
    )
  });
}

#[test]
fn emit_creates_expected_opcode() {
  do_with_parser("1 + 1", |mut parser| {
    parser.emit(1, OpCode::Add);
    do_with_ctx(parser, |ctx| {
      assert_eq!(ctx.instructions[0], OpCode::Add);
      assert_eq!(
        ctx.meta.get(0).unwrap(),
        OpCodeReflection {
          file: String::from("test"),
          source_line: String::from("1 + 1"),
          line: 1,
          column: 3,
        }
      );
    });
  });
}

#[test]
fn emit_const_creates_expected_opcode() {
  do_with_parser("foo + 1", |mut parser| {
    parser.emit_const(2, Value::Num(1.0));
    do_with_ctx(parser, |ctx| {
      assert_eq!(ctx.instructions[0], OpCode::Const(0));
      assert_eq!(ctx.consts[0], Value::Num(1.0));
      assert_eq!(
        ctx.meta.get(0).unwrap(),
        OpCodeReflection {
          file: String::from("test"),
          source_line: String::from("foo + 1"),
          line: 1,
          column: 7,
        }
      );
    });
  });
}

#[test]
fn sync_advances_to_expected_index() {
  do_with_parser(SYNC_TEST_SCRIPT, |mut parser| {
    let check_current = |parser: &mut Parser, e: &mut f64| {
      if let Token::Number(n) = parser.current().unwrap() {
        assert_eq!(n, *e);
        *e += 1.0;
      } else {
        panic!("position incorrect: {}", parser.index);
      }
    };

    let mut check = 1.0;

    parser.sync();
    assert_eq!(parser.index, 3);
    check_current(&mut parser, &mut check);
    parser.advance();

    for n in 0..11 {
      parser.sync();
      assert_eq!(parser.index, 6 + n * 4);
      parser.advance();
      check_current(&mut parser, &mut check);
      parser.advance();
    }
  });
}
