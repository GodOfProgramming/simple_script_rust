use crate::expr::{Binary, Expr, Grouping, Literal, Unary, Visitor};
use crate::lex::{Token, TokenType, Value};

type ParseResult<T> = Result<Box<dyn Expr<'static, T>>, String>;

pub fn parse<T: 'static>(tokens: &Vec<Token>) -> Result<Box<dyn Expr<'static, T>>, String> {
  let mut current = 0usize;
  expression(tokens, &mut current)
}

fn expression<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  list(tokens, current)
}

fn list<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  left_associative_binary(tokens, current, equality, &[TokenType::Comma])
}

fn equality<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  left_associative_binary(
    tokens,
    current,
    comparison,
    &[TokenType::ExEq, TokenType::EqEq],
  )
}

fn comparison<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  left_associative_binary(
    tokens,
    current,
    addition,
    &[
      TokenType::GreaterThan,
      TokenType::GreaterEq,
      TokenType::LessThan,
      TokenType::LessEq,
    ],
  )
}

fn addition<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  left_associative_binary(
    tokens,
    current,
    multiplication,
    &[TokenType::Plus, TokenType::Minus],
  )
}

fn multiplication<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  left_associative_binary(
    tokens,
    current,
    unary,
    &[TokenType::Slash, TokenType::Asterisk],
  )
}

fn unary<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  right_associateive_unary(
    tokens,
    current,
    primary,
    &[TokenType::Exclamation, TokenType::Minus, TokenType::Plus],
  )
}

fn primary<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ParseResult<T> {
  if match_token(
    tokens,
    current,
    &[
      TokenType::False,
      TokenType::True,
      TokenType::Nil,
      TokenType::NumberLiteral,
      TokenType::StringLiteral,
    ],
  ) {
    let prev = previous(tokens, current);

    if let Some(v) = &prev.literal {
      return Ok(Box::new(Literal::new(Value::from(v))));
    }
  }

  if match_token(tokens, current, &[TokenType::LeftParen]) {
    let expr = expression(tokens, current)?;
    consume(
      tokens,
      current,
      &TokenType::RightParen,
      "Expect ')' after expression.",
    )?;
    return Ok(Box::new(Grouping::new(expr)));
  }

  // TODO proper error handling
  Err(format!(""))
}

fn left_associative_binary<T: 'static>(
  tokens: &Vec<Token>,
  current: &mut usize,
  next: fn(&Vec<Token>, &mut usize) -> ParseResult<T>,
  types: &[TokenType],
) -> ParseResult<T> {
  let mut expr = next(tokens, current)?;

  while match_token(tokens, current, types) {
    let op = previous(tokens, current);
    let right = next(tokens, current)?;
    expr = Box::new(Binary::new(expr, op.clone(), right));
  }

  Ok(expr)
}

fn right_associateive_unary<T: 'static>(
  tokens: &Vec<Token>,
  current: &mut usize,
  next: fn(&Vec<Token>, &mut usize) -> ParseResult<T>,
  types: &[TokenType],
) -> ParseResult<T> {
  if match_token(tokens, current, types) {
    let op = previous(tokens, current);
    let right = unary(tokens, current)?;
    Ok(Box::new(Unary::new(op.clone(), right)))
  } else {
    next(tokens, current)
  }
}

fn match_token(tokens: &Vec<Token>, current: &mut usize, types: &[TokenType]) -> bool {
  for token_type in types.iter() {
    if check(tokens, current, token_type) {
      advance(tokens, current);
      return true;
    }
  }

  false
}

fn check(tokens: &Vec<Token>, current: &usize, t: &TokenType) -> bool {
  !is_at_end(tokens, current) && peek(tokens, current).token_type == *t
}

fn advance<'a>(tokens: &'a Vec<Token>, current: &mut usize) -> &'a Token {
  if !is_at_end(tokens, current) {
    *current += 1;
  }

  previous(tokens, current)
}

fn is_at_end(tokens: &Vec<Token>, current: &usize) -> bool {
  peek(tokens, current).token_type == TokenType::Eof
}

fn peek<'a>(tokens: &'a Vec<Token>, current: &usize) -> &'a Token {
  &tokens[*current]
}

fn previous<'a>(tokens: &'a Vec<Token>, current: &usize) -> &'a Token {
  &tokens[current - 1]
}

fn consume<'a>(
  tokens: &'a Vec<Token>,
  current: &mut usize,
  token_type: &TokenType,
  msg: &'static str,
) -> Result<&'a Token, String> {
  if check(tokens, current, token_type) {
    Ok(advance(tokens, current))
  } else {
    Err(format!("{}", msg))
  }
}

fn sync(tokens: &Vec<Token>, current: &mut usize) {
  advance(tokens, current);

  while !is_at_end(tokens, current) {
    if previous(tokens, current).token_type == TokenType::Semicolon {
      return;
    }

    match peek(tokens, current).token_type {
      TokenType::Class => return,
      TokenType::Fun => return,
      TokenType::Var => return,
      TokenType::For => return,
      TokenType::If => return,
      TokenType::While => return,
      TokenType::Print => return,
      TokenType::Return => return,
      _ => (),
    }

    advance(tokens, current);
  }
}

type EvalResult = Result<Value, String>;

pub fn eval(expr: Box<dyn Expr<EvalResult>>) -> EvalResult {
  let mut e = Evaluator::new();
  e.eval(&expr)
}

struct Evaluator;

impl Evaluator {
  fn new() -> Evaluator {
    Evaluator {}
  }

  pub fn eval(&mut self, expr: &Box<dyn Expr<EvalResult>>) -> EvalResult {
    expr.accept(self)
  }

  fn is_truthy(&mut self, v: Value) -> Value {
    if v == Value::Nil {
      return Value::Bool(false);
    }

    if let Value::Bool(_) = v {
      return v;
    }

    Value::Bool(true)
  }
}

impl Visitor<'_, EvalResult> for Evaluator {
  fn visit_binary_expr(&mut self, e: &Binary<EvalResult>) -> EvalResult {
    let left = self.eval(&e.left)?;
    let right = self.eval(&e.right)?;

    // value comparison
    if e.operator.token_type == TokenType::EqEq {
      return Ok(Value::Bool(left == right));
    }

    if e.operator.token_type == TokenType::ExEq {
      return Ok(Value::Bool(left != right));
    }

    // number arithmetic and comparison
    if let Value::Num(l) = left {
      if let Value::Num(r) = right {
        return match e.operator.token_type {
          TokenType::Plus => Ok(Value::Num(l + r)),
          TokenType::Minus => Ok(Value::Num(l - r)),
          TokenType::Slash => Ok(Value::Num(l / r)),
          TokenType::Asterisk => Ok(Value::Num(l * r)),
          TokenType::GreaterThan => Ok(Value::Bool(l > r)),
          TokenType::GreaterEq => Ok(Value::Bool(l >= r)),
          TokenType::LessThan => Ok(Value::Bool(l < r)),
          TokenType::LessEq => Ok(Value::Bool(l <= r)),
          _ => Err(format!(
            "Invalid operator ({:?}) for {} and {}",
            e.operator, l, r
          )),
        };
      }
    }

    // string concatenation
    if e.operator.token_type == TokenType::Plus {
      if let Value::Str(l) = left.clone() {
        if let Value::Str(r) = right {
          return Ok(Value::Str(format!("{}{}", l, r)));
        } else if let Value::Num(r) = right {
          return Ok(Value::Str(format!("{}{}", l, r)));
        }
      } else if let Value::Num(l) = left {
        if let Value::Str(r) = right {
          return Ok(Value::Str(format!("{}{}", l, r)));
        }
      }
    }

    Err(format!(
      "Combination of {:?} and {:?} not allowed with {:?}",
      left, right, e.operator
    ))
  }

  fn visit_grouping_expr(&mut self, e: &Grouping<EvalResult>) -> EvalResult {
    self.eval(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: &Literal<EvalResult>) -> EvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: &Unary<EvalResult>) -> EvalResult {
    let right = self.eval(&e.right)?;

    match e.operator.token_type {
      TokenType::Exclamation => Ok(self.is_truthy(right)),
      TokenType::Minus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(-n))
        } else {
          Err(format!(""))
        }
      }
      TokenType::Plus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(n.abs()))
        } else {
          Err(format!(""))
        }
      }
      _ => Err(format!("")),
    }
  }
}

pub struct Printer;

impl Printer {
  pub fn new() -> Printer {
    Printer {}
  }

  pub fn print(&mut self, expr: Box<dyn Expr<String>>) -> String {
    expr.accept(self)
  }

  fn parenthesize(&mut self, name: &String, exprs: &[&Box<dyn Expr<String>>]) -> String {
    let mut strings = Vec::new();
    strings.push(String::from("("));
    strings.push(name.clone());
    for expr in exprs.iter() {
      strings.push(String::from(" "));
      strings.push(expr.accept(self));
    }
    strings.push(String::from(")"));
    strings.join("")
  }
}

impl Visitor<'_, String> for Printer {
  fn visit_binary_expr(&mut self, e: &Binary<String>) -> String {
    if let Some(lexeme) = &e.operator.lexeme {
      self.parenthesize(&lexeme, &[&e.left, &e.right])
    } else {
      String::from("?")
    }
  }

  fn visit_grouping_expr(&mut self, e: &Grouping<String>) -> String {
    self.parenthesize(&String::from("group"), &[&e.expression])
  }

  fn visit_literal_expr(&mut self, e: &Literal<String>) -> String {
    match &e.value {
      Value::Nil => String::from("nil"),
      Value::Bool(b) => {
        if *b {
          String::from("true")
        } else {
          String::from("false")
        }
      }
      Value::Str(s) => s.clone(),
      Value::Num(f) => format!("{}", f),
    }
  }

  fn visit_unary_expr(&mut self, e: &Unary<String>) -> String {
    if let Some(lexeme) = &e.operator.lexeme {
      self.parenthesize(&lexeme, &[&e.right])
    } else {
      String::from("?")
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lex;

  const BASIC_MATH_SRC: &str = r#"1 / 2 * (3 + 4) + 1"#;

  // todo, define a visitor type that is configured to use a list
  // of expected tokens. then when visiting it iterates through that
  // list of tokens checking each node of the ast while traversing
  #[test]
  fn analyze_basic() {
    let (lines, tokens) = lex::analyze(BASIC_MATH_SRC).unwrap();
    let ast = parse::<()>(&tokens).unwrap();
  }
}
