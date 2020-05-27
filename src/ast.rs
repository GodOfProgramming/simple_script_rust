use crate::env::Environment;
use crate::expr::{self, Binary, Expr, Grouping, Literal, Unary, Variable};
use crate::lex::{Token, TokenType, Value};
use crate::stmt::{self, Expression, Print, Stmt, Var};
use std::marker::PhantomData;

type ParseResult<'a, T> = Result<Vec<Box<dyn Stmt<'a, T>>>, String>;
type StatementResult<'a, T> = Result<Box<dyn Stmt<'a, T>>, String>;
type ExprResult<'a, T> = Result<Box<dyn Expr<'a, T>>, String>;

pub fn parse<'a, T: 'a>(tokens: &'a Vec<Token>) -> ParseResult<'a, T> {
  let parser = Parser::<'a, T>::new(tokens);
  parser.parse()
}

struct Parser<'a> {
  tokens: &'a Vec<Token>,
  current: usize,
}

impl<'a> Parser<'a> {
  fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
    Parser {
      tokens,
      current: 0,
    }
  }

  fn parse<T>(&'a mut self) -> ParseResult<'a, T> {
    let mut current = 0usize;
    let mut statements = Vec::new();

    while !self.is_at_end() {
      statements.push(self.decl()?);
    }

    Ok(statements)
  }

  fn decl<T>(&'a mut self) -> StatementResult<'a, T> {
    match if self.match_token(&[TokenType::Var]) {
      self.var_decl()
    } else {
      self.statement()
    } {
      Ok(v) => Ok(v),
      Err(msg) => {
        self.sync();
        Err(msg)
      }
    }
  }

  fn var_decl<T>(&'a mut self) -> StatementResult<'a, T> {
    let name = self.consume(&TokenType::Identifier, "Expected variable name")?;
    let expr = None;
    if self.match_token(&[TokenType::Equal]) {
      expr = Some(self.expression()?);
    }

    self.consume<T>(&TokenType::Semicolon, "Expected ';' after variable decl")?;
    Ok(Box::new(Var::new(name.clone(), expr)))
  }

  fn statement<T>(&'a mut self) -> StatementResult<'a, T> {
    if self.match_token(&[TokenType::Print]) {
      self.print_statement()
    } else {
      self.expr_statement()
    }
  }

  fn print_statement<T>(&'a mut self) -> StatementResult<'a, T> {
    let expr = self.expression()?;
    self.consume(&TokenType::Semicolon, "expected ';' after value")?;
    Ok(Box::new(Print::new(expr)))
  }

  fn expr_statement<T>(&'a mut self) -> StatementResult<'a, T> {
    let expr = self.expression()?;
    self.consume(&TokenType::Semicolon, "expected ';' after value")?;
    Ok(Box::new(Expression::new(expr)))
  }

  fn expression<T>(&'a mut self) -> ExprResult<'a, T> {
    self.list()
  }

  fn list<T>(&'a mut self) -> ExprResult<'a, T> {
    self.left_associative_binary(Parser::equality, &[TokenType::Comma])
  }

  fn equality<T>(&'a mut self) -> ExprResult<'a, T> {
    self.left_associative_binary(Parser::comparison, &[TokenType::ExEq, TokenType::EqEq])
  }

  fn comparison<T>(&'a mut self) -> ExprResult<'a, T> {
    self.left_associative_binary(
      Parser::addition,
      &[
        TokenType::GreaterThan,
        TokenType::GreaterEq,
        TokenType::LessThan,
        TokenType::LessEq,
      ],
    )
  }

  fn addition<T>(&'a mut self) -> ExprResult<'a, T> {
    self.left_associative_binary(Parser::multiplication, &[TokenType::Plus, TokenType::Minus])
  }

  fn multiplication<T>(&'a mut self) -> ExprResult<'a, T> {
    self.left_associative_binary(Parser::unary, &[TokenType::Slash, TokenType::Asterisk])
  }

  fn unary<T>(&'a mut self) -> ExprResult<'a, T> {
    self.right_associateive_unary(
      Parser::primary,
      &[TokenType::Exclamation, TokenType::Minus, TokenType::Plus],
    )
  }

  fn primary<T>(&'a mut self) -> ExprResult<'a, T> {
    if self.match_token(&[
      TokenType::False,
      TokenType::True,
      TokenType::Nil,
      TokenType::NumberLiteral,
      TokenType::StringLiteral,
    ]) {
      let prev = self.previous();

      if let Some(v) = &prev.literal {
        return Ok(Box::new(Literal::new(Value::from(v))));
      }
    }

    if self.match_token(&[TokenType::Identifier]) {
      return Ok(Box::new(Variable::new(self.previous().clone())));
    }

    if self.match_token(&[TokenType::LeftParen]) {
      let expr = self.expression()?;
      self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
      return Ok(Box::new(Grouping::new(expr)));
    }

    // TODO proper error handling
    Err(format!(""))
  }

  fn left_associative_binary<T>(
    &'a mut self,
    next: fn(&'a mut Self) -> ExprResult<'a, T>,
    types: &[TokenType],
  ) -> ExprResult<'a, T> {
    let mut expr = next(self)?;

    while self.match_token(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Box::new(Binary::new(expr, op.clone(), right));
    }

    Ok(expr)
  }

  fn right_associateive_unary<T>(
    &'a mut self,
    next: fn(&'a mut Self) -> ExprResult<'a, T>,
    types: &[TokenType],
  ) -> ExprResult<'a, T> {
    if self.match_token(types) {
      let op = self.previous();
      let right = self.unary()?;
      Ok(Box::new(Unary::new(op.clone(), right)))
    } else {
      next(self)
    }
  }

  fn match_token(&'a mut self, types: &[TokenType]) -> bool {
    for token_type in types.iter() {
      if self.check(token_type) {
        self.advance();
        return true;
      }
    }

    false
  }

  fn check(&self, t: &TokenType) -> bool {
    !self.is_at_end() && self.peek().token_type == *t
  }

  fn advance(&'a mut self) -> &'a Token {
    if !self.is_at_end() {
      self.current += 1;
    }

    self.previous()
  }

  fn is_at_end(&self) -> bool {
    self.peek().token_type == TokenType::Eof
  }

  fn peek(&'a self) -> &'a Token {
    &self.tokens[self.current]
  }

  fn previous(&'a self) -> &'a Token {
    &self.tokens[self.current - 1]
  }

  fn consume(&'a mut self, token_type: &TokenType, msg: &'static str) -> Result<&'a Token, String> {
    if self.check(token_type) {
      Ok(self.advance())
    } else {
      Err(format!("{}", msg))
    }
  }

  fn sync(&'a mut self) {
    self.advance();

    while !self.is_at_end() {
      if self.previous().token_type == TokenType::Semicolon {
        return;
      }

      match self.peek().token_type {
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

      self.advance();
    }
  }
}

type EvalResult = Result<Value, String>;

pub fn exec<'a>(globals: &'a Environment, prgm: Vec<Box<dyn Stmt<'a, EvalResult>>>) -> EvalResult {
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

  pub fn exec(&mut self, stmt: &Box<dyn Stmt<'a, EvalResult>>) -> EvalResult {
    stmt.accept(self)
  }

  pub fn eval(&mut self, expr: &Box<dyn Expr<'a, EvalResult>>) -> EvalResult {
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

impl<'a> stmt::Visitor<'a, EvalResult> for Evaluator<'a> {
  fn visit_expression_stmt(&mut self, e: &Expression<'a, EvalResult>) -> EvalResult {
    self.eval(&e.expr)
  }

  fn visit_print_stmt(&mut self, e: &Print<'a, EvalResult>) -> EvalResult {
    let value = self.eval(&e.expr)?;
    println!("{}", value);
    Ok(Value::Nil)
  }

  fn visit_var_stmt(&mut self, e: &Var<'a, EvalResult>) -> EvalResult {
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

impl<'a> expr::Visitor<'a, EvalResult> for Evaluator<'a> {
  fn visit_binary_expr(&mut self, e: &Binary<'a, EvalResult>) -> EvalResult {
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

  fn visit_grouping_expr(&mut self, e: &Grouping<'a, EvalResult>) -> EvalResult {
    self.eval(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: &Literal<EvalResult>) -> EvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: &Unary<'a, EvalResult>) -> EvalResult {
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
