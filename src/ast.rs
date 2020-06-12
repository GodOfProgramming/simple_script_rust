use crate::complex::{CallErr, Closure, ScriptFunction};
use crate::env::{Env, EnvRef};
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GroupingExpr, LiteralExpr,
  LogicalExpr, RangeExpr, TernaryExpr, UnaryExpr, VariableExpr, Visitor as ExprVisitor,
};
use crate::lex::{Token, TokenType};
use crate::stmt::{
  self, BlockStmt, ExpressionStmt, FunctionStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, VarStmt,
  Visitor as StmtVisitor, WhileStmt,
};
use crate::types::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub struct AstErr {
  pub msg: String,
  pub line: usize,
}

impl From<CallErr> for AstErr {
  fn from(err: CallErr) -> Self {
    Self {
      msg: err.msg,
      line: err.line,
    }
  }
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
    match if self.match_token(vec![TokenType::Var]) {
      self.var_decl()
    } else if self.match_token(vec![TokenType::Fun]) {
      self.fun_decl("function")
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
    let name = self.consume(TokenType::Identifier, "Expected variable name")?;
    let mut expr = None;
    if self.match_token(vec![TokenType::Equal]) {
      expr = Some(Box::new(self.expression()?));
    }

    self.consume(TokenType::Semicolon, "expected ';' after variable decl")?;
    Ok(Stmt::new_var(name, expr))
  }

  fn fun_decl(&mut self, kind: &str) -> StatementResult {
    let name = self.consume(TokenType::Identifier, &format!("expect {} name", kind))?;
    self.consume(
      TokenType::LeftParen,
      &format!("expect '(' after {} name", kind),
    )?;
    let mut params = Vec::new();
    if !self.check(TokenType::RightParen) {
      loop {
        params.push(self.consume(TokenType::Identifier, "expected identifier")?);

        if !self.match_token(vec![TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen, "expect ')' after parameters")?;

    self.consume(
      TokenType::LeftBrace,
      &format!("Expect '{{' before {} body", kind),
    )?;

    let body = self.block()?;

    Ok(Stmt::new_function(name, Rc::new(params), Rc::new(body)))
  }

  fn statement(&mut self) -> StatementResult {
    if self.match_token(vec![TokenType::Print]) {
      self.print_statement()
    } else if self.match_token(vec![TokenType::Return]) {
      self.return_statement()
    } else if self.match_token(vec![TokenType::For]) {
      self.for_statement()
    } else if self.match_token(vec![TokenType::While]) {
      self.while_statement()
    } else if self.match_token(vec![TokenType::If]) {
      self.if_statement()
    } else if self.match_token(vec![TokenType::LeftBrace]) {
      Ok(Stmt::new_block(self.block()?))
    } else {
      self.expr_statement()
    }
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_print(Box::new(expr)))
  }

  fn return_statement(&mut self) -> StatementResult {
    let keyword = self.previous();
    let mut value = None;
    if !self.check(TokenType::Semicolon) {
      value = Some(Box::new(self.expression()?));
    }

    self.consume(TokenType::Semicolon, "expected ';' after return value")?;
    return Ok(Stmt::new_return(keyword, value));
  }

  fn for_statement(&mut self) -> StatementResult {
    let token = self.previous();

    let initializer = if self.match_token(vec![TokenType::Semicolon]) {
      None
    } else if self.match_token(vec![TokenType::Var]) {
      Some(self.var_decl()?)
    } else {
      Some(self.expr_statement()?)
    };

    let mut condition = if self.check(TokenType::Semicolon) {
      None
    } else {
      Some(self.expression()?)
    };

    self.consume(TokenType::Semicolon, "Expect ';' after loop condition")?;

    let increment = if self.check(TokenType::Semicolon) {
      None
    } else {
      Some(self.expression()?)
    };

    self.consume(TokenType::LeftBrace, "Expect '{' after increment")?;

    let mut body = Stmt::new_block(self.block()?);

    if let Some(inc) = increment {
      body = Stmt::new_block(vec![body, Stmt::new_expression(Box::new(inc))]);
    }

    if let None = condition {
      condition = Some(Expr::new_literal(Value::Bool(true)));
    }

    body = Stmt::new_while(token, Box::new(condition.unwrap()), Box::new(body));

    if let Some(init) = initializer {
      body = Stmt::new_block(vec![init, body])
    }

    Ok(body)
  }

  fn while_statement(&mut self) -> StatementResult {
    let token = self.previous();
    let condition = self.expression()?;
    self.consume(TokenType::LeftBrace, "missing '{' after if condition")?;
    let body = Stmt::new_block(self.block()?);
    Ok(Stmt::new_while(token, Box::new(condition), Box::new(body)))
  }

  fn if_statement(&mut self) -> StatementResult {
    let condition = self.expression()?;
    self.consume(TokenType::LeftBrace, "missing '{' after if condition")?;
    let if_true = Stmt::new_block(self.block()?);
    let mut if_false = None;

    if self.match_token(vec![TokenType::Else]) {
      if_false = if self.match_token(vec![TokenType::LeftBrace]) {
        Some(Box::new(Stmt::new_block(self.block()?)))
      } else if self.match_token(vec![TokenType::If]) {
        Some(Box::new(self.if_statement()?))
      } else {
        return Err(AstErr {
          msg: format!("invalid token after token {}", self.peek()),
          line: self.peek().line,
        });
      };
    }

    Ok(Stmt::new_if(
      Box::new(condition),
      Box::new(if_true),
      if_false,
    ))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_expression(Box::new(expr)))
  }

  fn expression(&mut self) -> ExprResult {
    self.ternary()
  }

  fn _list(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.match_token(vec![TokenType::Comma]) {
      let op = self.previous();
      let right = self._list()?;
      expr = Expr::new_binary(Box::new(expr), op, Box::new(right));
    }

    Ok(expr)
  }

  fn ternary(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.match_token(vec![TokenType::Conditional]) {
      let if_true = self.expression()?;
      self.consume(TokenType::Colon, "expected ':' for conditional operator")?;
      let if_false = self.expression()?;
      expr = Expr::new_ternary(Box::new(expr), Box::new(if_true), Box::new(if_false));
    }

    Ok(expr)
  }

  fn assignment(&mut self) -> ExprResult {
    let expr = self.range()?;

    if self.match_token(vec![TokenType::Equal]) {
      let equals = self.previous();
      let value = self.assignment()?;

      if let Expr::Variable(v) = expr {
        Ok(Expr::new_assign(v.name, Box::new(value)))
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

    if self.match_token(vec![TokenType::Range]) {
      let token = self.previous();
      let end = self.or()?;
      begin = Expr::new_range(Box::new(begin), token, Box::new(end));
    }

    Ok(begin)
  }

  fn or(&mut self) -> ExprResult {
    self.left_associative_logical(Parser::and, vec![TokenType::Or])
  }

  fn and(&mut self) -> ExprResult {
    self.left_associative_logical(Parser::equality, vec![TokenType::And])
  }

  fn equality(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::comparison, vec![TokenType::ExEq, TokenType::EqEq])
  }

  fn comparison(&mut self) -> ExprResult {
    self.left_associative_binary(
      Parser::addition,
      vec![
        TokenType::GreaterThan,
        TokenType::GreaterEq,
        TokenType::LessThan,
        TokenType::LessEq,
      ],
    )
  }

  fn addition(&mut self) -> ExprResult {
    self.left_associative_binary(
      Parser::multiplication,
      vec![TokenType::Plus, TokenType::Minus],
    )
  }

  fn multiplication(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::unary, vec![TokenType::Slash, TokenType::Asterisk])
  }

  fn unary(&mut self) -> ExprResult {
    self.right_associative_unary(
      Parser::call,
      vec![TokenType::Exclamation, TokenType::Minus, TokenType::Plus],
    )
  }

  fn call(&mut self) -> ExprResult {
    let mut expr = self.primary()?;

    while self.match_token(vec![TokenType::LeftParen]) {
      expr = self.finish_call(expr)?;
    }

    Ok(expr)
  }

  fn primary(&mut self) -> ExprResult {
    if self.match_token(vec![
      TokenType::False,
      TokenType::True,
      TokenType::Nil,
      TokenType::NumberLiteral,
      TokenType::StringLiteral,
    ]) {
      let prev = self.previous();

      if let Some(v) = &prev.literal {
        return Ok(Expr::new_literal(Value::from(v)));
      }
    }

    if self.match_token(vec![TokenType::Identifier]) {
      return Ok(Expr::new_variable(self.previous()));
    }

    if self.match_token(vec![TokenType::LeftParen]) {
      let expr = self.expression()?;
      self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
      return Ok(Expr::new_grouping(Box::new(expr)));
    }

    if self.match_token(vec![TokenType::Pipe]) {
      let mut params = Vec::new();
      if !self.check(TokenType::Pipe) {
        loop {
          params.push(self.consume(TokenType::Identifier, "expected identifier")?);

          if !self.match_token(vec![TokenType::Comma]) {
            break;
          }
        }
      }

      self.consume(TokenType::Pipe, "expect '|' after closure parameters")?;

      self.consume(TokenType::LeftBrace, "expect '{{' before closure body")?;

      let body = self.block()?;

      return Ok(Expr::new_closure(Rc::new(params), Rc::new(body)));
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
    types: Vec<TokenType>,
  ) -> ExprResult {
    let mut expr = next(self)?;

    // TODO stop cloning
    while self.match_token(types.clone()) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_logical(Box::new(expr), op, Box::new(right));
    }

    Ok(expr)
  }

  fn left_associative_binary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: Vec<TokenType>,
  ) -> ExprResult {
    let mut expr = next(self)?;

    // TODO stop cloning
    while self.match_token(types.clone()) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_binary(Box::new(expr), op, Box::new(right));
    }

    Ok(expr)
  }

  fn right_associative_unary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: Vec<TokenType>,
  ) -> ExprResult {
    if self.match_token(types) {
      let op = self.previous();
      let right = self.unary()?;
      Ok(Expr::new_unary(op, Box::new(right)))
    } else {
      next(self)
    }
  }

  fn block(&mut self) -> Result<Vec<Stmt>, AstErr> {
    let mut v = Vec::new();

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      v.push(self.decl()?);
    }

    self.consume(TokenType::RightBrace, "Expect '}' after block")?;

    Ok(v)
  }

  fn match_token(&mut self, types: Vec<TokenType>) -> bool {
    for token_type in types.into_iter() {
      if self.check(token_type) {
        self.advance();
        return true;
      }
    }

    false
  }

  fn check(&self, t: TokenType) -> bool {
    !self.is_at_end() && self.peek().token_type == t
  }

  fn advance(&mut self) -> Token {
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

  fn previous(&self) -> Token {
    self.tokens[self.current - 1].clone()
  }

  fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<Token, AstErr> {
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

  fn finish_call(&mut self, callee: Expr) -> ExprResult {
    let mut args = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        args.push(self.expression()?);

        if !self.match_token(vec![TokenType::Comma]) {
          break;
        }
      }
    }

    let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments")?;

    Ok(Expr::new_call(Box::new(callee), paren, args))
  }
}

pub enum StatementType {
  Regular(Value),
  Return(Value),
}

type ExprEvalResult = Result<Value, AstErr>;
pub type StmtEvalResult = Result<StatementType, AstErr>;

pub fn exec(globals: EnvRef, prgm: Vec<Stmt>) -> ExprEvalResult {
  let mut e = Evaluator::new(globals);
  let mut res = StatementType::Regular(Value::Nil);

  for stmt in prgm.into_iter() {
    res = e.eval_stmt(stmt)?;
  }

  match res {
    StatementType::Regular(v) => Ok(v),
    StatementType::Return(v) => Ok(v),
  }
}

pub struct Evaluator {
  pub current_env: EnvRef,
}

impl<'a> Evaluator {
  fn new(globals: EnvRef) -> Self {
    Self {
      current_env: globals,
    }
  }

  fn eval_expr(&mut self, e: Expr) -> ExprEvalResult {
    expr::accept(e, self)
  }

  fn eval_expr_ref(&mut self, e: &Expr) -> ExprEvalResult {
    expr::accept_ref(e, self)
  }

  fn eval_stmt(&mut self, s: Stmt) -> StmtEvalResult {
    stmt::accept(s, self)
  }

  fn eval_stmt_ref(&mut self, s: &Stmt) -> StmtEvalResult {
    stmt::accept_ref(s, self)
  }

  fn eval_block(&mut self, statements: Vec<Stmt>, env: EnvRef) -> StmtEvalResult {
    let prev_env = Rc::clone(&self.current_env);
    self.current_env = env;
    let mut result = Ok(StatementType::Regular(Value::Nil));

    for stmt in statements.into_iter() {
      match self.eval_stmt(stmt) {
        Ok(v) => match v {
          StatementType::Regular(v) => result = Ok(StatementType::Regular(v)),
          StatementType::Return(v) => {
            result = Ok(StatementType::Return(v));
            break;
          }
        },
        Err(s) => {
          result = Err(s);
          break;
        }
      };
    }

    self.current_env = prev_env;

    result
  }

  pub fn eval_block_ref(&mut self, statements: &Vec<Stmt>, env: EnvRef) -> StmtEvalResult {
    let prev_env = Rc::clone(&self.current_env);
    self.current_env = env;
    let mut result = Ok(StatementType::Regular(Value::Nil));

    for stmt in statements.iter() {
      match self.eval_stmt_ref(stmt) {
        Ok(v) => match v {
          StatementType::Regular(v) => result = Ok(StatementType::Regular(v)),
          StatementType::Return(v) => {
            result = Ok(StatementType::Return(v));
            break;
          }
        },
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

impl StmtVisitor<StmtEvalResult> for Evaluator {
  fn visit_expression_stmt(&mut self, e: ExpressionStmt) -> StmtEvalResult {
    Ok(StatementType::Regular(self.eval_expr(*e.expr)?))
  }

  fn visit_expression_stmt_ref(&mut self, e: &ExpressionStmt) -> StmtEvalResult {
    Ok(StatementType::Regular(self.eval_expr_ref(&e.expr)?))
  }

  fn visit_print_stmt(&mut self, e: PrintStmt) -> StmtEvalResult {
    println!("{}", self.eval_expr(*e.expr)?);
    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_print_stmt_ref(&mut self, e: &PrintStmt) -> StmtEvalResult {
    println!("{}", self.eval_expr_ref(&e.expr)?);
    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_var_stmt(&mut self, e: VarStmt) -> StmtEvalResult {
    let mut value = Value::Nil;

    if let Some(i) = e.initializer {
      value = self.eval_expr(*i)?;
    }

    match e.name.lexeme {
      Some(l) => self.current_env.borrow_mut().define(l, value),
      None => {
        return Err(AstErr {
          msg: String::from("missing variable name"),
          line: e.name.line,
        })
      }
    }

    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_var_stmt_ref(&mut self, e: &VarStmt) -> StmtEvalResult {
    let mut value = Value::Nil;

    if let Some(i) = &e.initializer {
      value = self.eval_expr_ref(i)?;
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

    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_block_stmt(&mut self, e: BlockStmt) -> StmtEvalResult {
    self.eval_block(
      e.statements,
      Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(
        &self.current_env,
      )))),
    )
  }

  fn visit_block_stmt_ref(&mut self, e: &BlockStmt) -> StmtEvalResult {
    self.eval_block_ref(
      &e.statements,
      Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(
        &self.current_env,
      )))),
    )
  }

  fn visit_if_stmt(&mut self, e: IfStmt) -> StmtEvalResult {
    let result = self.eval_expr(*e.condition)?;
    if self.is_truthy(&result) {
      self.eval_stmt(*e.if_true)
    } else if let Some(if_false) = e.if_false {
      self.eval_stmt(*if_false)
    } else {
      Ok(StatementType::Regular(Value::Nil))
    }
  }

  fn visit_if_stmt_ref(&mut self, e: &IfStmt) -> StmtEvalResult {
    let result = self.eval_expr_ref(&e.condition)?;
    if self.is_truthy(&result) {
      self.eval_stmt_ref(&e.if_true)
    } else if let Some(if_false) = &e.if_false {
      self.eval_stmt_ref(&if_false)
    } else {
      Ok(StatementType::Regular(Value::Nil))
    }
  }

  fn visit_while_stmt(&mut self, e: WhileStmt) -> StmtEvalResult {
    let mut result = StatementType::Regular(Value::Nil);
    if let Stmt::Block(blk) = *e.body {
      loop {
        let res = self.eval_expr_ref(&e.condition)?;
        if !self.is_truthy(&res) {
          break;
        }
        match self.visit_block_stmt_ref(&blk)? {
          StatementType::Regular(v) => result = StatementType::Regular(v),
          StatementType::Return(v) => {
            result = StatementType::Return(v);
            break;
          }
        }
      }
    } else {
      return Err(AstErr {
        msg: String::from("body of while loop is not block statement"),
        line: e.token.line,
      });
    }

    Ok(result)
  }

  fn visit_while_stmt_ref(&mut self, e: &WhileStmt) -> StmtEvalResult {
    let mut result = StatementType::Regular(Value::Nil);
    if let Stmt::Block(blk) = &*e.body {
      loop {
        let res = self.eval_expr_ref(&e.condition)?;
        if !self.is_truthy(&res) {
          break;
        }
        match self.visit_block_stmt_ref(&blk)? {
          StatementType::Regular(v) => result = StatementType::Regular(v),
          StatementType::Return(v) => {
            result = StatementType::Return(v);
            break;
          }
        }
      }
    } else {
      return Err(AstErr {
        msg: String::from("body of while loop is not block statement"),
        line: e.token.line,
      });
    }

    Ok(result)
  }

  fn visit_function_stmt(&mut self, e: FunctionStmt) -> StmtEvalResult {
    let name = e.name.lexeme.as_ref().unwrap().clone();
    let func = ScriptFunction::new(e);
    self
      .current_env
      .borrow_mut()
      .define(name, Value::Callee(Rc::new(func)));
    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_function_stmt_ref(&mut self, e: &FunctionStmt) -> StmtEvalResult {
    let name = e.name.lexeme.as_ref().unwrap().clone();
    let func = ScriptFunction::new(FunctionStmt::new(
      e.name.clone(),
      Rc::clone(&e.params),
      Rc::clone(&e.body),
    ));
    self
      .current_env
      .borrow_mut()
      .define(name, Value::Callee(Rc::new(func)));
    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_return_stmt(&mut self, s: ReturnStmt) -> StmtEvalResult {
    let mut value = Value::Nil;
    if let Some(e) = s.value {
      value = self.eval_expr(*e)?;
    }

    Ok(StatementType::Return(value))
  }

  fn visit_return_stmt_ref(&mut self, s: &ReturnStmt) -> StmtEvalResult {
    let mut value = Value::Nil;
    if let Some(e) = &s.value {
      value = self.eval_expr_ref(e)?;
    }

    Ok(StatementType::Return(value))
  }
}

impl ExprVisitor<ExprEvalResult> for Evaluator {
  fn visit_binary_expr(&mut self, e: BinaryExpr) -> ExprEvalResult {
    let left = self.eval_expr(*e.left)?;
    let right = self.eval_expr(*e.right)?;

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
      if let Value::Str(l) = &left {
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

  fn visit_binary_expr_ref(&mut self, e: &BinaryExpr) -> ExprEvalResult {
    let left = self.eval_expr_ref(&e.left)?;
    let right = self.eval_expr_ref(&e.right)?;

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
      if let Value::Str(l) = &left {
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

  fn visit_ternary_expr(&mut self, e: TernaryExpr) -> ExprEvalResult {
    let result = self.eval_expr(*e.condition)?;

    if self.is_truthy(&result) {
      self.eval_expr(*e.if_true)
    } else {
      self.eval_expr(*e.if_false)
    }
  }

  fn visit_ternary_expr_ref(&mut self, e: &TernaryExpr) -> ExprEvalResult {
    let result = self.eval_expr_ref(&e.condition)?;

    if self.is_truthy(&result) {
      self.eval_expr_ref(&e.if_true)
    } else {
      self.eval_expr_ref(&e.if_false)
    }
  }

  fn visit_grouping_expr(&mut self, e: GroupingExpr) -> ExprEvalResult {
    self.eval_expr(*e.expression)
  }

  fn visit_grouping_expr_ref(&mut self, e: &GroupingExpr) -> ExprEvalResult {
    self.eval_expr_ref(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: LiteralExpr) -> ExprEvalResult {
    Ok(e.value)
  }

  fn visit_literal_expr_ref(&mut self, e: &LiteralExpr) -> ExprEvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: UnaryExpr) -> ExprEvalResult {
    let right = self.eval_expr(*e.right)?;

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

  fn visit_unary_expr_ref(&mut self, e: &UnaryExpr) -> ExprEvalResult {
    let right = self.eval_expr_ref(&e.right)?;

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

  fn visit_variable_expr(&mut self, e: VariableExpr) -> ExprEvalResult {
    match e.name.lexeme {
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

  fn visit_variable_expr_ref(&mut self, e: &VariableExpr) -> ExprEvalResult {
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

  fn visit_assign_expr(&mut self, e: AssignExpr) -> ExprEvalResult {
    let value = self.eval_expr(*e.value)?;
    match e.name.lexeme {
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

  fn visit_assign_expr_ref(&mut self, e: &AssignExpr) -> ExprEvalResult {
    let value = self.eval_expr_ref(&e.value)?;
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

  fn visit_logical_expr(&mut self, e: LogicalExpr) -> ExprEvalResult {
    let left = self.eval_expr(*e.left)?;

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

    self.eval_expr(*e.right)
  }

  fn visit_logical_expr_ref(&mut self, e: &LogicalExpr) -> ExprEvalResult {
    let left = self.eval_expr_ref(&e.left)?;

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

    self.eval_expr_ref(&e.right)
  }

  fn visit_range_expr(&mut self, e: RangeExpr) -> ExprEvalResult {
    let begin = self.eval_expr(*e.begin)?;
    let end = self.eval_expr(*e.end)?;

    if let Value::Num(begin) = begin {
      if let Value::Num(end) = end {
        return Ok(Value::List(
          ((begin.round() as i64)..(end.round() as i64))
            .map(|n| Value::Num(n as f64))
            .collect(),
        ));
      }
    }

    Err(AstErr {
      msg: String::from("expected number with range expression"),
      line: e.token.line,
    })
  }

  fn visit_range_expr_ref(&mut self, e: &RangeExpr) -> ExprEvalResult {
    let begin = self.eval_expr_ref(&e.begin)?;
    let end = self.eval_expr_ref(&e.end)?;

    if let Value::Num(begin) = begin {
      if let Value::Num(end) = end {
        return Ok(Value::List(
          ((begin.round() as i64)..(end.round() as i64))
            .map(|n| Value::Num(n as f64))
            .collect(),
        ));
      }
    }

    Err(AstErr {
      msg: String::from("expected number with range expression"),
      line: e.token.line,
    })
  }

  fn visit_call_expr(&mut self, e: CallExpr) -> ExprEvalResult {
    let callee = self.eval_expr(*e.callee)?;

    if let Value::Callee(func) = callee {
      let mut args = Vec::new();
      for arg in e.args.into_iter() {
        args.push(self.eval_expr(arg)?);
      }
      Ok(func.call(self, args)?)
    } else {
      Err(AstErr {
        msg: format!("can't call type {}", callee),
        line: e.paren.line,
      })
    }
  }

  fn visit_call_expr_ref(&mut self, e: &CallExpr) -> ExprEvalResult {
    let callee = self.eval_expr_ref(&e.callee)?;

    if let Value::Callee(func) = callee {
      let mut args = Vec::new();
      for arg in e.args.iter() {
        args.push(self.eval_expr_ref(arg)?);
      }
      Ok(func.call(self, args)?)
    } else {
      Err(AstErr {
        msg: format!("can't call type {}", callee),
        line: e.paren.line,
      })
    }
  }

  fn visit_closure_expr(&mut self, e: ClosureExpr) -> ExprEvalResult {
    Ok(Value::Callee(Rc::new(Closure::new(
      e,
      Rc::clone(&self.current_env),
    ))))
  }

  fn visit_closure_expr_ref(&mut self, e: &ClosureExpr) -> ExprEvalResult {
    Ok(Value::Callee(Rc::new(Closure::new(
      ClosureExpr::new(Rc::clone(&e.params), Rc::clone(&e.body)),
      Rc::clone(&self.current_env),
    ))))
  }
}
