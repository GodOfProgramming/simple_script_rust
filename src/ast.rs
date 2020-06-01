use crate::env::{Env, EnvRef};
use crate::expr::{
  AssignExpr, BinaryExpr, Expr, GroupingExpr, LiteralExpr, LogicalExpr, RangeExpr, TernaryExpr,
  UnaryExpr, VariableExpr, Visitor as ExprVisitor,
};
use crate::lex::{Token, TokenType, Value};
use crate::stmt::{
  BlockStmt, ExpressionStmt, IfStmt, PrintStmt, Stmt, VarStmt, Visitor as StmtVisitor, WhileStmt,
};
use std::cell::RefCell;
use std::rc::Rc;

pub struct AstErr {
  pub msg: String,
  pub line: usize,
}

type ParseResult = Result<Vec<Stmt>, AstErr>;
type StatementResult = Result<Stmt, AstErr>;
type ExprResult = Result<Expr, AstErr>;

pub fn parse<'a>(tokens: &'a Vec<Token>) -> ParseResult {
  let mut parser = Parser::<'a>::new(tokens);
  parser.parse()
}

struct Parser<'a> {
  tokens: &'a Vec<Token>,
  current: usize,
}

impl<'a> Parser<'a> {
  fn new(tokens: &'a Vec<Token>) -> Self {
    Self { tokens, current: 0 }
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
    } else if self.match_token(&[TokenType::For]) {
      self.for_statement()
    } else if self.match_token(&[TokenType::While]) {
      self.while_statement()
    } else if self.match_token(&[TokenType::If]) {
      self.if_statement()
    } else if self.match_token(&[TokenType::LeftBrace]) {
      Ok(Stmt::Block(Box::new(BlockStmt::new(self.block()?))))
    } else {
      self.expr_statement()
    }
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(&TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::Print(Box::new(PrintStmt::new(expr))))
  }

  fn for_statement(&mut self) -> StatementResult {
    let token = self.previous();

    let initializer = if self.match_token(&[TokenType::Semicolon]) {
      None
    } else if self.match_token(&[TokenType::Var]) {
      Some(self.var_decl()?)
    } else {
      Some(self.expr_statement()?)
    };

    let mut condition = if self.check(&TokenType::Semicolon) {
      None
    } else {
      Some(self.expression()?)
    };

    self.consume(&TokenType::Semicolon, "Expect ';' after loop condition")?;

    let increment = if self.check(&TokenType::Semicolon) {
      None
    } else {
      Some(self.expression()?)
    };

    self.consume(&TokenType::LeftBrace, "Expect '{' after increment")?;

    let mut body = Stmt::Block(Box::new(BlockStmt::new(self.block()?)));

    if let Some(inc) = increment {
      body = Stmt::Block(Box::new(BlockStmt::new(vec![
        body,
        Stmt::Expression(Box::new(ExpressionStmt::new(inc))),
      ])));
    }

    if let None = condition {
      condition = Some(Expr::Literal(Box::new(LiteralExpr::new(Value::Bool(true)))));
    }

    body = Stmt::While(Box::new(WhileStmt::new(
      token.clone(),
      condition.unwrap(),
      body,
    )));

    if let Some(init) = initializer {
      body = Stmt::Block(Box::new(BlockStmt::new(vec![init, body])))
    }

    Ok(body)
  }

  fn while_statement(&mut self) -> StatementResult {
    let token = self.previous();
    let condition = self.expression()?;
    self.consume(&TokenType::LeftBrace, "missing '{' after if condition")?;
    let body = Stmt::Block(Box::new(BlockStmt::new(self.block()?)));
    Ok(Stmt::While(Box::new(WhileStmt::new(
      token.clone(),
      condition,
      body,
    ))))
  }

  fn if_statement(&mut self) -> StatementResult {
    let condition = self.expression()?;
    self.consume(&TokenType::LeftBrace, "missing '{' after if condition")?;
    let if_true = Stmt::Block(Box::new(BlockStmt::new(self.block()?)));
    let mut if_false = None;

    if self.match_token(&[TokenType::Else]) {
      if_false = if self.match_token(&[TokenType::LeftBrace]) {
        Some(Stmt::Block(Box::new(BlockStmt::new(self.block()?))))
      } else if self.match_token(&[TokenType::If]) {
        Some(self.if_statement()?)
      } else {
        return Err(AstErr {
          msg: format!("invalid token after token {}", self.peek()),
          line: self.peek().line,
        });
      };
    }

    Ok(Stmt::If(Box::new(IfStmt::new(
      condition, if_true, if_false,
    ))))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(&TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::Expression(Box::new(ExpressionStmt::new(expr))))
  }

  fn expression(&mut self) -> ExprResult {
    self.ternary()
  }

  fn _list(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.match_token(&[TokenType::Comma]) {
      let op = self.previous();
      let right = self._list()?;
      expr = Expr::Binary(Box::new(BinaryExpr::new(expr, op.clone(), right)));
    }

    Ok(expr)
  }

  fn ternary(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.match_token(&[TokenType::Conditional]) {
      let if_true = self.expression()?;
      self.consume(&TokenType::Colon, "expected ':' for conditional operator")?;
      let if_false = self.expression()?;
      expr = Expr::Ternary(Box::new(TernaryExpr::new(expr, if_true, if_false)));
    }

    Ok(expr)
  }

  fn assignment(&mut self) -> ExprResult {
    let expr = self.range()?;

    if self.match_token(&[TokenType::Equal]) {
      let equals = self.previous();
      let value = self.assignment()?;

      if let Expr::Variable(v) = expr {
        Ok(Expr::Assign(Box::new(AssignExpr::new(v.name, value))))
      } else {
        Err(AstErr {
          msg: String::from("invalid assignment target"),
          line: equals.line,
        })
      }
    } else {
      Ok(expr)
    }
  }

  fn range(&mut self) -> ExprResult {
    let mut begin = self.or()?;

    if self.match_token(&[TokenType::Range]) {
      let token = self.previous();
      let end = self.or()?;
      begin = Expr::Range(Box::new(RangeExpr::new(begin, token.clone(), end)));
    }

    Ok(begin)
  }

  fn or(&mut self) -> ExprResult {
    self.left_associative_logical(Parser::and, &[TokenType::Or])
  }

  fn and(&mut self) -> ExprResult {
    self.left_associative_logical(Parser::equality, &[TokenType::And])
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

    Err(AstErr {
      msg: String::from("could not find valid primary token"),
      line: self.peek().line,
    })
  }

  fn left_associative_logical(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenType],
  ) -> ExprResult {
    let mut expr = next(self)?;

    while self.match_token(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::Logical(Box::new(LogicalExpr::new(expr, op.clone(), right)));
    }

    Ok(expr)
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

  fn block(&mut self) -> Result<Vec<Stmt>, AstErr> {
    let mut v = Vec::new();

    while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
      v.push(self.decl()?);
    }

    self.consume(&TokenType::RightBrace, "Expect '}' after block")?;

    Ok(v)
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

  fn consume(&mut self, token_type: &TokenType, msg: &'static str) -> Result<&'a Token, AstErr> {
    if self.check(token_type) {
      Ok(self.advance())
    } else {
      Err(AstErr {
        msg: String::from(msg),
        line: self.peek().line,
      })
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

type EvalResult = Result<Value, AstErr>;

pub fn exec(globals: EnvRef, prgm: Vec<Stmt>) -> EvalResult {
  let mut e = Evaluator::new(globals);
  let mut res = Value::Nil;

  for stmt in prgm.iter() {
    res = e.eval_stmt(stmt)?;
  }

  Ok(res)
}

struct Evaluator {
  current_env: EnvRef,
}

impl<'a> Evaluator {
  fn new(globals: EnvRef) -> Self {
    Self {
      current_env: globals,
    }
  }

  fn eval_stmt(&mut self, stmt: &'a Stmt) -> EvalResult {
    stmt.accept(self)
  }

  fn eval_expr(&mut self, expr: &'a Expr) -> EvalResult {
    expr.accept(self)
  }

  fn eval_block(&mut self, statements: &Vec<Stmt>, env: EnvRef) -> EvalResult {
    let prev_env = Rc::clone(&self.current_env);
    self.current_env = env;
    let mut result = Ok(Value::Nil);

    for stmt in statements.iter() {
      match self.eval_stmt(&stmt) {
        Ok(v) => result = Ok(v),
        Err(s) => {
          result = Err(s);
          break;
        }
      };
    }

    self.current_env = prev_env;

    result
  }

  fn is_truthy(&mut self, v: &Value) -> bool {
    if *v == Value::Nil {
      return false;
    }

    if let Value::Bool(b) = *v {
      return b;
    }

    true
  }
}

impl StmtVisitor<EvalResult> for Evaluator {
  fn visit_expression_stmt(&mut self, e: &ExpressionStmt) -> EvalResult {
    self.eval_expr(&e.expr)
  }

  fn visit_print_stmt(&mut self, e: &PrintStmt) -> EvalResult {
    let value = self.eval_expr(&e.expr)?;
    println!("{}", value);
    Ok(Value::Nil)
  }

  fn visit_var_stmt(&mut self, e: &VarStmt) -> EvalResult {
    let mut value = Value::Nil;

    if let Some(i) = &e.initializer {
      value = self.eval_expr(&i)?;
    }

    match &e.name.lexeme {
      Some(l) => self.current_env.borrow_mut().define(l.clone(), value),
      None => {
        return Err(AstErr {
          msg: String::from("missing variable name"),
          line: e.name.line,
        })
      }
    }

    Ok(Value::Nil)
  }

  fn visit_block_stmt(&mut self, e: &BlockStmt) -> EvalResult {
    self.eval_block(
      &e.statements,
      Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(
        &self.current_env,
      )))),
    )
  }

  fn visit_if_stmt(&mut self, e: &IfStmt) -> EvalResult {
    let result = self.eval_expr(&e.condition)?;
    if self.is_truthy(&result) {
      self.eval_stmt(&e.if_true)
    } else if let Some(if_false) = &e.if_false {
      self.eval_stmt(&if_false)
    } else {
      Ok(Value::Nil)
    }
  }

  fn visit_while_stmt(&mut self, e: &WhileStmt) -> EvalResult {
    let mut conclusion = Value::Nil;
    if let Stmt::Block(blk) = &e.body {
      loop {
        let result = self.eval_expr(&e.condition)?;
        if !self.is_truthy(&result) {
          break;
        }
        conclusion = self.visit_block_stmt(blk)?;
      }
    } else {
      return Err(AstErr {
        msg: String::from("body of while loop is not block statement"),
        line: e.token.line,
      });
    }

    Ok(conclusion)
  }
}

impl ExprVisitor<EvalResult> for Evaluator {
  fn visit_binary_expr(&mut self, e: &BinaryExpr) -> EvalResult {
    let left = self.eval_expr(&e.left)?;
    let right = self.eval_expr(&e.right)?;

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
          _ => Err(AstErr {
            msg: format!("Invalid operator ({:?}) for {} and {}", e.operator, l, r),
            line: e.operator.line,
          }),
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

    Err(AstErr {
      msg: format!(
        "Combination of {:?} and {:?} not allowed with {:?}",
        left, right, e.operator
      ),
      line: e.operator.line,
    })
  }

  fn visit_ternary_expr(&mut self, e: &TernaryExpr) -> EvalResult {
    let result = self.eval_expr(&e.condition)?;

    if self.is_truthy(&result) {
      self.eval_expr(&e.if_true)
    } else {
      self.eval_expr(&e.if_false)
    }
  }

  fn visit_grouping_expr(&mut self, e: &GroupingExpr) -> EvalResult {
    self.eval_expr(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: &LiteralExpr) -> EvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: &UnaryExpr) -> EvalResult {
    let right = self.eval_expr(&e.right)?;

    match e.operator.token_type {
      TokenType::Exclamation => Ok(Value::Bool(!self.is_truthy(&right))),
      TokenType::Minus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(-n))
        } else {
          Err(AstErr {
            msg: format!("invalid negation on type {}", right),
            line: e.operator.line,
          })
        }
      }
      TokenType::Plus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(n.abs()))
        } else if let Value::Str(s) = right {
          match s.parse() {
            Ok(n) => Ok(Value::Num(n)),
            Err(err) => Err(AstErr {
              msg: format!("string parse error: {}", err),
              line: e.operator.line,
            }),
          }
        } else {
          Err(AstErr {
            msg: format!("invalid absolution on type {}", right),
            line: e.operator.line,
          })
        }
      }
      _ => Err(AstErr {
        msg: format!("invalid unary operator {}", e.operator),
        line: e.operator.line,
      }),
    }
  }

  fn visit_variable_expr(&mut self, e: &VariableExpr) -> EvalResult {
    match &e.name.lexeme {
      Some(l) => match self.current_env.borrow().lookup(&l) {
        Some(v) => Ok(v.clone()),
        None => Err(AstErr {
          msg: format!("used uninitialized variable '{}'", l),
          line: e.name.line,
        }),
      },
      None => Err(AstErr {
        msg: String::from("cannot declare unnamed var"),
        line: e.name.line,
      }),
    }
  }

  fn visit_assign_expr(&mut self, e: &AssignExpr) -> EvalResult {
    let value = self.eval_expr(&e.value)?;
    match &e.name.lexeme {
      Some(l) => {
        if let Err(msg) = self
          .current_env
          .borrow_mut()
          .assign(l.clone(), value.clone())
        {
          return Err(AstErr {
            msg: format!("assignment error: {}", msg),
            line: e.name.line,
          });
        }
      }
      None => {
        return Err(AstErr {
          msg: format!("cannot assign unnamed var"),
          line: e.name.line,
        })
      }
    }
    Ok(value)
  }

  fn visit_logical_expr(&mut self, e: &LogicalExpr) -> EvalResult {
    let left = self.eval_expr(&e.left)?;

    match e.operator.token_type {
      TokenType::Or => {
        if self.is_truthy(&left) {
          return Ok(left);
        }
      }
      TokenType::And => {
        if !self.is_truthy(&left) {
          return Ok(left);
        }
      }
      _ => {
        return Err(AstErr {
          msg: String::from("invalid attempt for logical comparison"),
          line: e.operator.line,
        })
      }
    }

    self.eval_expr(&e.right)
  }

  fn visit_range_expr(&mut self, e: &RangeExpr) -> EvalResult {
    let begin = self.eval_expr(&e.begin)?;
    let end = self.eval_expr(&e.end)?;

    if let Value::Num(begin) = begin {
      if let Value::Num(end) = end {
        return Ok(Value::List(((begin.round() as i64)..(end.round() as i64)).map(|n| Value::Num(n as f64)).collect()))
      }
    }

    Err(AstErr {
      msg: String::from("expected number with range expression"),
      line: e.token.line,
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::lex;

  const BASIC_MATH_SRC: &str = r#"1 / 2 * (3 + 4) + 1;"#;

  // todo, define a visitor type that is configured to use a list
  // of expected tokens. then when visiting it iterates through that
  // list of tokens checking each node of the ast while traversing
  #[test]
  fn parse_basic() {
    lex::analyze(BASIC_MATH_SRC).unwrap();
  }
}
