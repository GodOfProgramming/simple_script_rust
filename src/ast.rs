use crate::env::EnvRef;
use crate::expr::{self, BinaryExpr, Expr, GroupingExpr, LiteralExpr, UnaryExpr, VariableExpr};
use crate::lex::{Token, TokenType, Value};
use crate::stmt::{self, ExpressionStmt, PrintStmt, Stmt, VarStmt};
use std::marker::PhantomData;

type ParseResult = Result<Vec<Stmt>, String>;
type StatementResult = Result<Stmt, String>;
type ExprResult = Result<Expr, String>;

pub fn parse<'a>(tokens: &'a Vec<Token>) -> ParseResult {
  let mut parser = Parser::<'a>::new(tokens);
  parser.parse()
}

struct Parser<'a> {
  tokens: &'a Vec<Token>,
  current: usize,
}

impl<'a> Parser<'a> {
  fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
    Parser { tokens, current: 0 }
  }

  fn parse(&mut self) -> ParseResult {
    let mut statements = Vec::new();

    while !self.is_at_end() {
      statements.push(self.decl()?);
    }

    Ok(statements)
  }

  fn decl(&mut self) -> StatementResult {
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

  fn var_decl(&mut self) -> StatementResult {
    let name = self.consume(&TokenType::Identifier, "Expected variable name")?;
    let mut expr = None;
    if self.match_token(&[TokenType::Equal]) {
      expr = Some(self.expression()?);
    }

    self.consume(&TokenType::Semicolon, "Expected ';' after variable decl")?;
    Ok(Stmt::Var(Box::new(VarStmt::new(name.clone(), expr))))
  }

  fn statement(&mut self) -> StatementResult {
    if self.match_token(&[TokenType::Print]) {
      self.print_statement()
    } else {
      self.expr_statement()
    }
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(&TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::Print(Box::new(PrintStmt::new(expr))))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(&TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::Expression(Box::new(ExpressionStmt::new(expr))))
  }

  fn expression(&mut self) -> ExprResult {
    self.list()
  }

  fn list(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::equality, &[TokenType::Comma])
  }

  fn equality(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::comparison, &[TokenType::ExEq, TokenType::EqEq])
  }

  fn comparison(&mut self) -> ExprResult {
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

  fn addition(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::multiplication, &[TokenType::Plus, TokenType::Minus])
  }

  fn multiplication(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::unary, &[TokenType::Slash, TokenType::Asterisk])
  }

  fn unary(&mut self) -> ExprResult {
    self.right_associateive_unary(
      Parser::primary,
      &[TokenType::Exclamation, TokenType::Minus, TokenType::Plus],
    )
  }

  fn primary(&mut self) -> ExprResult {
    if self.match_token(&[
      TokenType::False,
      TokenType::True,
      TokenType::Nil,
      TokenType::NumberLiteral,
      TokenType::StringLiteral,
    ]) {
      let prev = self.previous();

      if let Some(v) = &prev.literal {
        return Ok(Expr::Literal(Box::new(LiteralExpr::new(Value::from(v)))));
      }
    }

    if self.match_token(&[TokenType::Identifier]) {
      return Ok(Expr::Variable(Box::new(VariableExpr::new(
        self.previous().clone(),
      ))));
    }

    if self.match_token(&[TokenType::LeftParen]) {
      let expr = self.expression()?;
      self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
      return Ok(Expr::Grouping(Box::new(GroupingExpr::new(expr))));
    }

    // TODO proper error handling
    Err(format!(""))
  }

  fn left_associative_binary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenType],
  ) -> ExprResult {
    let mut expr = next(self)?;

    while self.match_token(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::Binary(Box::new(BinaryExpr::new(expr, op.clone(), right)));
    }

    Ok(expr)
  }

  fn right_associateive_unary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenType],
  ) -> ExprResult {
    if self.match_token(types) {
      let op = self.previous();
      let right = self.unary()?;
      Ok(Expr::Unary(Box::new(UnaryExpr::new(op.clone(), right))))
    } else {
      next(self)
    }
  }

  fn match_token(&mut self, types: &[TokenType]) -> bool {
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

  fn advance(&mut self) -> &'a Token {
    if !self.is_at_end() {
      self.current += 1;
    }

    self.previous()
  }

  fn is_at_end(&self) -> bool {
    self.peek().token_type == TokenType::Eof
  }

  fn peek(&self) -> &'a Token {
    &self.tokens[self.current]
  }

  fn previous(&self) -> &'a Token {
    &self.tokens[self.current - 1]
  }

  fn consume(&mut self, token_type: &TokenType, msg: &'static str) -> Result<&'a Token, String> {
    if self.check(token_type) {
      Ok(self.advance())
    } else {
      Err(format!("{}", msg))
    }
  }

  fn sync(&mut self) {
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

pub fn exec(globals: EnvRef, prgm: Vec<Stmt>) -> EvalResult {
  let mut e = Evaluator::new(globals);
  let mut res = Value::Nil;

  for stmt in prgm.iter() {
    res = e.exec(stmt)?;
  }

  Ok(res)
}

struct Evaluator {
  globals: EnvRef,
}

impl<'a> Evaluator {
  fn new(globals: EnvRef) -> Evaluator {
    Evaluator { globals }
  }

  fn exec(&mut self, stmt: &'a Stmt) -> EvalResult {
    stmt.accept(self)
  }

  fn eval(&mut self, expr: &'a Expr) -> EvalResult {
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

impl stmt::Visitor<EvalResult> for Evaluator {
  fn visit_expression_stmt(&mut self, e: &ExpressionStmt) -> EvalResult {
    self.eval(&e.expr)
  }

  fn visit_print_stmt(&mut self, e: &PrintStmt) -> EvalResult {
    let value = self.eval(&e.expr)?;
    println!("{}", value);
    Ok(Value::Nil)
  }

  fn visit_var_stmt(&mut self, e: &VarStmt) -> EvalResult {
    let mut value = Value::Nil;

    if let Some(i) = &e.initializer {
      value = self.eval(&i)?;
    }

    match &e.name.lexeme {
      Some(l) => self.globals.borrow_mut().define(&l, value),
      None => return Err(String::from("missing variable name")),
    }

    Ok(Value::Nil)
  }
}

impl expr::Visitor<EvalResult> for Evaluator {
  fn visit_binary_expr(&mut self, e: &BinaryExpr) -> EvalResult {
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

  fn visit_grouping_expr(&mut self, e: &GroupingExpr) -> EvalResult {
    self.eval(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: &LiteralExpr) -> EvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: &UnaryExpr) -> EvalResult {
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

  fn visit_variable_expr(&mut self, e: &VariableExpr) -> EvalResult {
    match &e.name.lexeme {
      Some(l) => match self.globals.borrow().lookup(&l) {
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
    let ast = parse(&tokens).unwrap();
  }
}
