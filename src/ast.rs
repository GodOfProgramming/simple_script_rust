use crate::env::Environment;
use crate::expr::{self, Binary, Expr, Grouping, Literal, Unary, Variable};
use crate::lex::{Token, TokenType, Value};
use crate::stmt::{self, Expression, Print, Stmt, Var};

type ParseResult<T> = Result<Vec<Box<dyn Stmt<'static, T>>>, String>;
type StatementResult<T> = Result<Box<dyn Stmt<'static, T>>, String>;
type ExprResult<T> = Result<Box<dyn Expr<'static, T>>, String>;

pub fn parse<T: 'static>(tokens: &Vec<Token>) -> ParseResult<T> {
  let mut current = 0usize;
  let mut statements = Vec::new();

  while !is_at_end(tokens, &current) {
    statements.push(decl(tokens, &mut current)?);
  }

  Ok(statements)
}

fn decl<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> StatementResult<T> {
  match if match_token(tokens, current, &[TokenType::Var]) {
    var_decl(tokens, current)
  } else {
    statement(tokens, current)
  } {
    Ok(v) => Ok(v),
    Err(msg) => {
      sync(tokens, current);
      Err(msg)
    }
  }
}

fn var_decl<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> StatementResult<T> {
  let name = consume(
    tokens,
    current,
    &TokenType::Identifier,
    "Expected variable name",
  )?;
  let expr = None;
  if match_token(tokens, current, &[TokenType::Equal]) {
    expr = Some(expression(tokens, current)?);
  }

  consume(
    tokens,
    current,
    &TokenType::Semicolon,
    "Expected ';' after variable declaration",
  )?;
  Ok(Box::new(Var::new(name.clone(), expr)))
}

fn statement<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> StatementResult<T> {
  if match_token(tokens, current, &[TokenType::Print]) {
    print_statement(tokens, current)
  } else {
    expr_statement(tokens, current)
  }
}

fn print_statement<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> StatementResult<T> {
  let expr = expression(tokens, current)?;
  consume(
    tokens,
    current,
    &TokenType::Semicolon,
    "expected ';' after value",
  )?;
  Ok(Box::new(Print::new(expr)))
}

fn expr_statement<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> StatementResult<T> {
  let expr = expression(tokens, current)?;
  consume(
    tokens,
    current,
    &TokenType::Semicolon,
    "expected ';' after value",
  )?;
  Ok(Box::new(Expression::new(expr)))
}

fn expression<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
  list(tokens, current)
}

fn list<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
  left_associative_binary(tokens, current, equality, &[TokenType::Comma])
}

fn equality<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
  left_associative_binary(
    tokens,
    current,
    comparison,
    &[TokenType::ExEq, TokenType::EqEq],
  )
}

fn comparison<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
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

fn addition<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
  left_associative_binary(
    tokens,
    current,
    multiplication,
    &[TokenType::Plus, TokenType::Minus],
  )
}

fn multiplication<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
  left_associative_binary(
    tokens,
    current,
    unary,
    &[TokenType::Slash, TokenType::Asterisk],
  )
}

fn unary<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
  right_associateive_unary(
    tokens,
    current,
    primary,
    &[TokenType::Exclamation, TokenType::Minus, TokenType::Plus],
  )
}

fn primary<T: 'static>(tokens: &Vec<Token>, current: &mut usize) -> ExprResult<T> {
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

  if match_token(tokens, current, &[TokenType::Identifier]) {
    return Ok(Box::new(Variable::new(previous(tokens, current).clone())));
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
  next: fn(&Vec<Token>, &mut usize) -> ExprResult<T>,
  types: &[TokenType],
) -> ExprResult<T> {
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
  next: fn(&Vec<Token>, &mut usize) -> ExprResult<T>,
  types: &[TokenType],
) -> ExprResult<T> {
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

pub fn exec<'a>(globals: &'a Environment, prgm: Vec<Box<dyn Stmt<EvalResult>>>) -> EvalResult {
  let mut e = Evaluator::new(globals);
  let mut res = Value::Nil;

  for stmt in prgm.iter() {
    res = e.exec(stmt)?;
  }

  Ok(res)
}

struct Evaluator<'a> {
  globals: &'a Environment,
}

impl<'a> Evaluator<'a> {
  fn new(globals: &'a Environment) -> Evaluator {
    Evaluator { globals }
  }

  pub fn exec(&mut self, stmt: &Box<dyn Stmt<EvalResult>>) -> EvalResult {
    stmt.accept(self)
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

impl stmt::Visitor<'_, EvalResult> for Evaluator {
  fn visit_expression_stmt(&mut self, e: &Expression<EvalResult>) -> EvalResult {
    self.eval(&e.expr)
  }

  fn visit_print_stmt(&mut self, e: &Print<EvalResult>) -> EvalResult {
    let value = self.eval(&e.expr)?;
    println!("{}", value);
    Ok(Value::Nil)
  }

  fn visit_var_stmt(&mut self, e: &Var<EvalResult>) -> EvalResult {
    let mut value = Value::Nil;

    if let Some(i) = e.initializer {
      value = self.eval(&i)?;
    }

    match e.name.lexeme {
      Some(l) => self.globals.define(&l, value),
      None => return Err(String::from("missing variable name")),
    }

    Ok(Value::Nil)
  }
}

impl expr::Visitor<'_, EvalResult> for Evaluator {
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

  fn visit_variable_expr(&mut self, e: &Variable<EvalResult>) -> EvalResult {
    match e.name.lexeme {
      Some(l) => match self.globals.lookup(&l) {
        Some(v) => Ok(v.clone()),
        None => Err(String::from("used uninitialized variable")),
      },
      None => Err(String::from("cannot declare var with token")),
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
