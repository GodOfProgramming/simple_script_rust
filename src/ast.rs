use crate::env::EnvRef;
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GetExpr, GroupingExpr, IsExpr,
  LiteralExpr, LogicalExpr, RangeExpr, SetExpr, TernaryExpr, UnaryExpr, VariableExpr,
};
use crate::lex::{self, Token, TokenKind};
use crate::res;
use crate::stmt::{
  self, BlockStmt, ClassStmt, ExpressionStmt, FunctionStmt, IfStmt, LetStmt, LoadStmt, LoadrStmt,
  PrintStmt, ReturnStmt, Stmt, WhileStmt,
};
use crate::types::{Class, Function, Instance, New, Value, ValueError, Visitor};
use crate::ScriptError;
use std::collections::HashMap;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

type ParseResult = Result<Vec<Stmt>, ScriptError>;
type StatementResult = Result<Stmt, ScriptError>;
type ExprResult = Result<Expr, ScriptError>;

macro_rules! check {
  ($this:ident, $types:pat) => {
    !$this.is_at_end() && matches!($this.peek().kind, $types)
  }
}

// if the token type matches, advance
macro_rules! consume {
  ($this:ident, $types:pat, $alt:block) => {
    if check!($this, $types) {
      Ok($this.advance())
    } else $alt
  }
}

macro_rules! advance_if_matches {
  ($this:ident, $types:pat) => {
    if check!($this, $types) {
      $this.advance();
      true
    } else {
      false
    }
  }
}

pub fn parse(file_id: usize, tokens: &[Token]) -> ParseResult {
  let mut parser = Parser::new(file_id, tokens);
  parser.parse()
}

struct Parser<'tokens> {
  file_id: usize,
  tokens: &'tokens [Token],
  current: usize,
  current_id: usize,
}

impl<'tokens> Parser<'tokens> {
  fn new(file_id: usize, tokens: &'tokens [Token]) -> Self {
    Self {
      file_id,
      tokens,
      current: 0,
      current_id: 0,
    }
  }

  fn next_id(&mut self) -> usize {
    let id = self.current_id;
    self.current_id += 1;
    id
  }

  fn parse(&mut self) -> ParseResult {
    let mut statements = Vec::new();

    while !self.is_at_end() {
      statements.push(self.decl()?);
    }

    Ok(statements)
  }

  fn decl(&mut self) -> StatementResult {
    match self.statement() {
      Ok(v) => Ok(v),
      Err(msg) => {
        self.sync();
        Err(msg)
      }
    }
  }

  fn statement(&mut self) -> StatementResult {
    if self.advance_if_match(&[TokenKind::Let]) {
      self.var_decl()
    } else if self.advance_if_match(&[TokenKind::Fn]) {
      self.fn_decl("function")
    } else if self.advance_if_match(&[TokenKind::Class]) {
      self.class_decl()
    } else if self.advance_if_match(&[TokenKind::Print]) {
      self.print_statement()
    } else if self.advance_if_match(&[TokenKind::Return]) {
      self.return_statement()
    } else if self.advance_if_match(&[TokenKind::For]) {
      self.for_statement()
    } else if self.advance_if_match(&[TokenKind::While]) {
      self.while_statement()
    } else if self.advance_if_match(&[TokenKind::If]) {
      self.if_statement()
    } else if self.advance_if_match(&[TokenKind::LeftBrace]) {
      Ok(Stmt::new_block(self.block()?, self.next_id()))
    } else if self.advance_if_match(&[TokenKind::Load]) {
      self.load_statement()
    } else if self.advance_if_match(&[TokenKind::Loadr]) {
      self.loadr_statement()
    } else {
      self.expr_statement()
    }
  }

  fn var_decl(&mut self) -> StatementResult {
    let name = consume!(self, TokenKind::Identifier(_), {
      self.make_err("expected variable name")
    })?;
    let mut expr = None;
    if self.advance_if_match(&[TokenKind::Equal]) {
      expr = Some(self.expression()?);
    }

    consume!(self, TokenKind::Semicolon, {
      self.make_err("expected ';' after variable decl")
    })?;
    Ok(Stmt::new_let(name, expr, self.next_id()))
  }

  fn fn_decl(&mut self, kind: &str) -> StatementResult {
    let name = consume!(self, TokenKind::Identifier(_), {
      self.make_err(&format!("expect {} name", kind))
    })?;
    consume!(self, TokenKind::LeftParen, {
      self.make_err(&format!("expect '(' after {} name", kind))
    })?;
    let mut params = Vec::new();
    if !self.check(TokenKind::RightParen) {
      loop {
        params.push(consume!(self, TokenKind::Identifier(_), {
          self.make_err("expected identifier")
        })?);

        if !self.advance_if_match(&[TokenKind::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenKind::RightParen, "expect ')' after parameters")?;

    self.consume(
      TokenKind::LeftBrace,
      &format!("Expect '{{' before {} body", kind),
    )?;

    let body = self.block()?;

    Ok(Stmt::new_function(
      name,
      Rc::new(params),
      Rc::new(body),
      self.next_id(),
    ))
  }

  fn class_decl(&mut self) -> StatementResult {
    let name = self.consume(TokenKind::Identifier, "expect class name")?;
    self.consume(TokenKind::LeftBrace, "expect '{' before class body")?;

    let mut methods = Vec::new();
    while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
      if self.advance_if_match(&[TokenKind::Fn]) {
        methods.push(self.fn_decl("method")?);
      } else {
        return Err(ScriptError {
          file: self.file.clone(),
          line: name.line,
          msg: String::from("invalid token found"),
        });
      }
    }

    self.consume(TokenKind::RightBrace, "expect '}' after class body")?;

    Ok(Stmt::new_class(name, methods, self.next_id()))
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_print(expr, self.next_id()))
  }

  fn return_statement(&mut self) -> StatementResult {
    let keyword = self.previous();
    let mut value = None;
    if !self.check(TokenKind::Semicolon) {
      value = Some(self.expression()?);
    }

    self.consume(TokenKind::Semicolon, "expected ';' after return value")?;
    Ok(Stmt::new_return(keyword, value, self.next_id()))
  }

  fn for_statement(&mut self) -> StatementResult {
    let token = self.previous();

    let initializer = if self.advance_if_match(&[TokenKind::Let]) {
      self.var_decl()?
    } else {
      self.expr_statement()?
    };

    let condition = self.expression()?;
    self.consume(TokenKind::Semicolon, "Expect ';' after condition")?;

    let increment = self.expression()?;
    self.consume(TokenKind::LeftBrace, "Expect '{' after increment")?;

    let body = Stmt::new_block(
      vec![
        initializer,
        Stmt::new_while(
          token,
          condition,
          vec![
            Stmt::new_block(self.block()?, self.next_id()),
            Stmt::new_expression(increment, self.next_id()),
          ],
          self.next_id(),
        ),
      ],
      self.next_id(),
    );

    Ok(body)
  }

  fn while_statement(&mut self) -> StatementResult {
    let token = self.previous();
    let condition = self.expression()?;
    self.consume(TokenKind::LeftBrace, "missing '{' after if condition")?;
    let body = self.block()?;
    Ok(Stmt::new_while(token, condition, body, self.next_id()))
  }

  fn if_statement(&mut self) -> StatementResult {
    let condition = self.expression()?;
    self.consume(TokenKind::LeftBrace, "missing '{' after if condition")?;
    let if_true = self.block()?;
    let mut if_false = None;

    if self.advance_if_match(&[TokenKind::Else]) {
      if_false = if self.advance_if_match(&[TokenKind::LeftBrace]) {
        Some(Box::new(Stmt::new_block(self.block()?, self.next_id())))
      } else if self.advance_if_match(&[TokenKind::If]) {
        Some(Box::new(self.if_statement()?))
      } else {
        return Err(ScriptError {
          file: self.file.clone(),
          line: self.peek().line,
          msg: format!("invalid token after token {}", self.peek()),
        });
      };
    }

    Ok(Stmt::new_if(condition, if_true, if_false, self.next_id()))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_expression(expr, self.next_id()))
  }

  fn load_statement(&mut self) -> StatementResult {
    let load = self.previous();
    let file = self.expression()?;
    self.consume(TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_load(load, file, self.next_id()))
  }

  fn loadr_statement(&mut self) -> StatementResult {
    let loadr = self.previous();
    let file = self.expression()?;
    self.consume(TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_loadr(loadr, file, self.next_id()))
  }

  fn expression(&mut self) -> ExprResult {
    self.is()
  }

  fn is(&mut self) -> ExprResult {
    let mut expr = self.ternary()?;

    if self.advance_if_match(&[TokenKind::Is]) {
      let token = self.previous();
      if self.advance_if_match(&[
        TokenKind::Nil,
        TokenKind::Error,
        TokenKind::Bool,
        TokenKind::Number,
        TokenKind::String,
        TokenKind::List,
        TokenKind::Fn,
        TokenKind::Class,
      ]) {
        let datatype = self.previous();
        expr = Expr::Is(IsExpr::new(Box::new(expr), token, datatype, self.next_id()))
      }
    }

    Ok(expr)
  }

  // TODO
  fn _list(&mut self) -> ExprResult {
    self.ternary()
  }

  fn ternary(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.advance_if_match(&[TokenKind::Conditional]) {
      let if_true = self.expression()?;
      self.consume(TokenKind::Colon, "expected ':' for conditional operator")?;
      let if_false = self.expression()?;
      expr = Expr::new_ternary(
        Box::new(expr),
        Box::new(if_true),
        Box::new(if_false),
        self.next_id(),
      );
    }

    Ok(expr)
  }

  fn assignment(&mut self) -> ExprResult {
    let expr = self.range()?;

    if self.advance_if_match(&[TokenKind::Equal]) {
      let equals = self.previous();
      let value = self.assignment()?;

      if let Expr::Variable(v) = expr {
        Ok(Expr::new_assign(v.name, Box::new(value), self.next_id()))
      } else if let Expr::Get(g) = expr {
        Ok(Expr::new_set(
          g.object,
          g.name,
          Box::new(value),
          self.next_id(),
        ))
      } else {
        Err(ScriptError {
          file: self.file.clone(),
          line: equals.line,
          msg: String::from("invalid assignment target"),
        })
      }
    } else {
      Ok(expr)
    }
  }

  fn range(&mut self) -> ExprResult {
    let mut begin = self.or()?;

    if self.advance_if_match(&[TokenKind::Range]) {
      let token = self.previous();
      let end = self.or()?;
      begin = Expr::new_range(Box::new(begin), token, Box::new(end), self.next_id());
    }

    Ok(begin)
  }

  fn or(&mut self) -> ExprResult {
    self.left_associative_logical(Parser::and, &[TokenKind::Or])
  }

  fn and(&mut self) -> ExprResult {
    self.left_associative_logical(Parser::equality, &[TokenKind::And])
  }

  fn equality(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::comparison, &[TokenKind::ExEq, TokenKind::EqEq])
  }

  fn comparison(&mut self) -> ExprResult {
    self.left_associative_binary(
      Parser::addition,
      &[
        TokenKind::GreaterThan,
        TokenKind::GreaterEq,
        TokenKind::LessThan,
        TokenKind::LessEq,
      ],
    )
  }

  fn addition(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::multiplication, &[TokenKind::Plus, TokenKind::Minus])
  }

  fn multiplication(&mut self) -> ExprResult {
    self.left_associative_binary(Parser::unary, &[TokenKind::Slash, TokenKind::Asterisk])
  }

  fn unary(&mut self) -> ExprResult {
    self.right_associative_unary(
      Parser::call,
      &[TokenKind::Exclamation, TokenKind::Minus, TokenKind::Plus],
    )
  }

  fn call(&mut self) -> ExprResult {
    let mut expr = self.primary()?;

    loop {
      if self.advance_if_match(&[TokenKind::LeftParen]) {
        expr = self.finish_call(expr)?;
      } else if self.advance_if_match(&[TokenKind::Dot]) {
        let name = self.consume(TokenKind::Identifier, "expected property name after '.'")?;
        expr = Expr::Get(GetExpr::new(Box::new(expr), name, self.next_id()));
      } else {
        break;
      }
    }

    Ok(expr)
  }

  fn primary(&mut self) -> ExprResult {
    if self.advance_if_match(&[
      TokenKind::False,
      TokenKind::True,
      TokenKind::Nil,
      TokenKind::NumberLiteral,
      TokenKind::StringLiteral,
    ]) {
      let prev = self.previous();

      if let Some(v) = &prev.literal {
        return Ok(Expr::new_literal(v.clone(), self.next_id()));
      }
    }

    if self.advance_if_match(&[TokenKind::Identifier]) {
      return Ok(Expr::new_variable(self.previous(), self.next_id()));
    }

    if self.advance_if_match(&[TokenKind::LeftParen]) {
      let expr = self.expression()?;
      self.consume(TokenKind::RightParen, "Expect ')' after expression.")?;
      return Ok(Expr::new_grouping(Box::new(expr), self.next_id()));
    }

    if self.advance_if_match(&[TokenKind::Pipe]) {
      let mut params = Vec::new();
      if !self.check(TokenKind::Pipe) {
        loop {
          params.push(self.consume(TokenKind::Identifier, "expected identifier")?);

          if !self.advance_if_match(&[TokenKind::Comma]) {
            break;
          }
        }
      }

      self.consume(TokenKind::Pipe, "expect '|' after closure parameters")?;

      self.consume(TokenKind::LeftBrace, "expect '{{' before closure body")?;

      let body = self.block()?;

      return Ok(Expr::new_closure(
        Rc::new(params),
        Rc::new(body),
        self.next_id(),
      ));
    }

    Err(ScriptError {
      file: self.file.clone(),
      line: self.peek().line,
      msg: String::from("could not find valid primary token"),
    })
  }

  fn left_associative_logical(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenKind],
  ) -> ExprResult {
    let mut expr = next(self)?;

    while self.advance_if_match(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_logical(Box::new(expr), op, Box::new(right), self.next_id());
    }

    Ok(expr)
  }

  fn left_associative_binary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenKind],
  ) -> ExprResult {
    let mut expr = next(self)?;

    while self.advance_if_match(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_binary(Box::new(expr), op, Box::new(right), self.next_id());
    }

    Ok(expr)
  }

  fn right_associative_unary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenKind],
  ) -> ExprResult {
    if self.advance_if_match(types) {
      let op = self.previous();
      let right = self.unary()?;
      Ok(Expr::new_unary(op, Box::new(right), self.next_id()))
    } else {
      next(self)
    }
  }

  fn block(&mut self) -> Result<Vec<Stmt>, ScriptError> {
    let mut v = Vec::new();

    while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
      v.push(self.decl()?);
    }

    self.consume(TokenKind::RightBrace, "Expect '}' after block")?;

    Ok(v)
  }

  fn advance_if_match(&mut self, types: &[TokenKind]) -> bool {
    for token_type in types.iter() {
      if self.check(*token_type) {
        self.advance();
        return true;
      }
    }

    false
  }

  fn advance(&mut self) -> Token {
    if !self.is_at_end() {
      self.current += 1;
    }

    self.previous()
  }

  fn is_at_end(&self) -> bool {
    self.peek().kind == TokenKind::Eof
  }

  fn peek(&self) -> &'tokens Token {
    &self.tokens[self.current]
  }

  fn previous(&self) -> Token {
    self.tokens[self.current - 1].clone()
  }

  fn sync(&mut self) {
    self.advance();

    while !self.is_at_end() {
      if self.previous().token_type == TokenKind::Semicolon {
        return;
      }

      match self.peek().token_type {
        TokenKind::Class => return,
        TokenKind::Fn => return,
        TokenKind::Let => return,
        TokenKind::For => return,
        TokenKind::If => return,
        TokenKind::While => return,
        TokenKind::Print => return,
        TokenKind::Return => return,
        _ => (),
      }

      self.advance();
    }
  }

  fn finish_call(&mut self, callee: Expr) -> ExprResult {
    let mut args = Vec::new();

    if !self.check(TokenKind::RightParen) {
      loop {
        args.push(self.expression()?);

        if !self.advance_if_match(&[TokenKind::Comma]) {
          break;
        }
      }
    }

    let paren = self.consume(TokenKind::RightParen, "Expect ')' after arguments")?;

    Ok(Expr::new_call(
      Box::new(callee),
      paren,
      args,
      self.next_id(),
    ))
  }

  fn make_err(&self, msg: &str) -> Result<Token, ScriptError> {
    Err(ScriptError {
      file_id: self.file_id,
      line: self.peek().line,
      msg: String::from(msg),
    })
  }
}

pub enum StatementType {
  Regular(Value),
  Return(Value),
}

type ExprEvalResult = Result<Value, ScriptError>;
pub type StmtEvalResult = Result<StatementType, ScriptError>;

pub fn exec(file: OsString, globals: EnvRef, prgm: Vec<Stmt>) -> ExprEvalResult {
  let mut e = Evaluator::new(file, globals.snapshot());
  res::resolve(&mut e, &prgm)?;
  let mut res = StatementType::Regular(Value::Nil);

  for stmt in prgm.iter() {
    res = e.eval_stmt(stmt)?;
  }

  match res {
    StatementType::Regular(v) => Ok(v),
    StatementType::Return(v) => Ok(v),
  }
}

pub struct Evaluator {
  pub file: OsString,
  pub env: EnvRef,
  pub last_object: Option<Value>,
  locals: HashMap<usize, usize>,
}

impl Evaluator {
  pub fn new(file: OsString, env: EnvRef) -> Self {
    Self {
      file,
      env,
      last_object: None,
      locals: HashMap::new(),
    }
  }

  fn eval_expr(&mut self, e: &Expr) -> ExprEvalResult {
    expr::accept(e, self)
  }

  fn eval_stmt(&mut self, s: &Stmt) -> StmtEvalResult {
    stmt::accept(s, self)
  }

  pub fn eval_block(&mut self, statements: &[Stmt], env: EnvRef) -> StmtEvalResult {
    let prev_env = self.env.snapshot();
    self.env = env;

    let mut result = Ok(StatementType::Regular(Value::Nil));

    for stmt in statements.iter() {
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

    self.env = prev_env;

    result
  }

  pub fn resolve(&mut self, id: usize, depth: usize) {
    self.locals.insert(id, depth);
  }

  pub fn lookup_variable(&self, name: &Token, id: usize) -> ExprEvalResult {
    match if let Some(depth) = self.locals.get(&id) {
      self.env.lookup_at(*depth, &name.lexeme)
    } else {
      self.env.lookup(&name.lexeme)
    } {
      Some(v) => Ok(v),
      None => Err(ScriptError {
        file: self.file.clone(),
        line: name.line,
        msg: format!("used uninitialized variable '{}'", name.lexeme),
      }),
    }
  }
}

impl Visitor<ExpressionStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &ExpressionStmt) -> StmtEvalResult {
    Ok(StatementType::Regular(self.eval_expr(&e.expr)?))
  }
}

impl Visitor<PrintStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &PrintStmt) -> StmtEvalResult {
    println!("{}", self.eval_expr(&e.expr)?);
    Ok(StatementType::Regular(Value::Nil))
  }
}

impl Visitor<LetStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &LetStmt) -> StmtEvalResult {
    let mut value = Value::Nil;

    if let Some(i) = &e.initializer {
      value = self.eval_expr(i)?;
    }

    self.env.define(&e.name.lexeme, value);

    Ok(StatementType::Regular(Value::Nil))
  }
}

impl Visitor<BlockStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &BlockStmt) -> StmtEvalResult {
    self.eval_block(
      &e.statements,
      EnvRef::new_with_enclosing(self.env.snapshot()),
    )
  }
}

impl Visitor<ClassStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, s: &ClassStmt) -> StmtEvalResult {
    self.env.define(&s.name.lexeme, Value::Nil);

    let mut static_methods = EnvRef::new_with_enclosing(self.env.snapshot());
    let mut instance_methods = EnvRef::new_with_enclosing(self.env.snapshot());

    // for every method in the class body
    for method in s.methods.iter() {
      // pull out the function from the method
      if let Stmt::Function(f) = method {
        // if it begins with an '@' it is static
        if f.name.lexeme.starts_with('@') {
          // function names must be longer than one character
          if f.name.lexeme.len() == 1 {
            return Err(ScriptError {
              file: self.file.clone(),
              line: f.name.line,
              msg: format!(
                "class method {} for class {} cannot be one char long if its only char is '@'",
                f.name.lexeme, s.name.lexeme,
              ),
            });
          }
          // strip off the '@'
          let name = &f.name.lexeme[1..];
          let func = Function::new_script(
            String::from(name),
            Rc::clone(&f.params),
            Rc::clone(&f.body),
            static_methods.snapshot(),
          );

          if static_methods.define(name, Value::new(func)) {
            return Err(ScriptError {
              file: self.file.clone(),
              line: f.name.line,
              msg: format!("redeclaration of class method {}", f.name.lexeme),
            });
          }
        } else {
          let name = &f.name.lexeme[..];
          let func = Function::new_method(
            f.name.lexeme.clone(),
            Rc::clone(&f.params),
            Rc::clone(&f.body),
            instance_methods.snapshot(),
          );
          if instance_methods.define(name, Value::new(func)) {
            return Err(ScriptError {
              file: self.file.clone(),
              line: f.name.line,
              msg: format!("redeclaration of class method {}", f.name.lexeme),
            });
          }
        };
      } else {
        return Err(ScriptError {
          file: self.file.clone(),
          line: s.name.line,
          msg: format!("invalid declaration inside <class {}> body", s.name.lexeme),
        });
      }
    }

    let class = Value::Class(Class::new(
      s.name.lexeme.clone(),
      static_methods,
      instance_methods,
    ));

    self
      .env
      .assign(s.name.lexeme.clone(), class)
      .map_err(|err| ScriptError {
        file: self.file.clone(),
        line: s.name.line,
        msg: format!("error assigning class: {}", err),
      })?;

    Ok(StatementType::Regular(Value::Nil))
  }
}

impl Visitor<IfStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &IfStmt) -> StmtEvalResult {
    let result = self.eval_expr(&e.condition)?;
    if result.truthy() {
      self.eval_block(&e.if_true, EnvRef::new_with_enclosing(self.env.snapshot()))
    } else if let Some(if_false) = &e.if_false {
      self.eval_stmt(&if_false)
    } else {
      Ok(StatementType::Regular(Value::Nil))
    }
  }
}

impl Visitor<WhileStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &WhileStmt) -> StmtEvalResult {
    let mut result = StatementType::Regular(Value::Nil);

    loop {
      let res = self.eval_expr(&e.condition)?;
      if !res.truthy() {
        break;
      }
      match self.eval_block(&e.body, EnvRef::new_with_enclosing(self.env.snapshot()))? {
        StatementType::Regular(v) => result = StatementType::Regular(v),
        StatementType::Return(v) => {
          result = StatementType::Return(v);
          break;
        }
      }
    }

    Ok(result)
  }
}

impl Visitor<FunctionStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &FunctionStmt) -> StmtEvalResult {
    let func = Function::new_script(
      e.name.lexeme.clone(),
      Rc::clone(&e.params),
      Rc::clone(&e.body),
      self.env.snapshot(),
    );
    self.env.define(&e.name.lexeme, Value::Callee(func));
    Ok(StatementType::Regular(Value::Nil))
  }
}

impl Visitor<ReturnStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, s: &ReturnStmt) -> StmtEvalResult {
    let mut value = Value::Nil;
    if let Some(e) = &s.value {
      value = self.eval_expr(e)?;
    }

    Ok(StatementType::Return(value))
  }
}

impl Visitor<LoadStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, s: &LoadStmt) -> StmtEvalResult {
    let path = self.eval_expr(&s.path)?;
    if let Value::Str(path) = path {
      match fs::read_to_string(&path) {
        Ok(contents) => {
          let tokens = lex::analyze(path.clone().into(), &contents)?;
          let program = parse(path.clone().into(), &tokens.tokens)?;
          let result = exec(path.into(), self.env.snapshot(), program)?;
          Ok(StatementType::Regular(result))
        }
        Err(err) => Err(ScriptError {
          file: self.file.clone(),
          line: s.load.line,
          msg: format!("failed loading file {}: {}", path, err),
        }),
      }
    } else {
      Err(ScriptError {
        file: self.file.clone(),
        line: s.load.line,
        msg: "cannot load non string value".to_string(),
      })
    }
  }
}

impl Visitor<LoadrStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, s: &LoadrStmt) -> StmtEvalResult {
    let path = self.eval_expr(&s.path)?;
    if let Value::Str(path) = path {
      let mut wd: PathBuf = env::current_dir().map_err(|_| ScriptError {
        file: self.file.clone(),
        line: s.loadr.line,
        msg: "unable to check current working directory".to_string(),
      })?;
      let mut fp = PathBuf::from(&self.file);
      fp.pop();
      wd.push(fp);
      wd.push(&path);
      match fs::read_to_string(&wd) {
        Ok(contents) => {
          let tokens = lex::analyze(path.clone().into(), &contents)?;
          let program = parse(path.clone().into(), &tokens.tokens)?;
          let result = exec(path.into(), self.env.snapshot(), program)?;
          Ok(StatementType::Regular(result))
        }
        Err(err) => Err(ScriptError {
          file: self.file.clone(),
          line: s.loadr.line,
          msg: format!("failed loading file {}: {}", wd.as_path().display(), err),
        }),
      }
    } else {
      Err(ScriptError {
        file: self.file.clone(),
        line: s.loadr.line,
        msg: "cannot load non string value".to_string(),
      })
    }
  }
}

impl Visitor<BinaryExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &BinaryExpr) -> ExprEvalResult {
    let left = self.eval_expr(&e.left)?;
    let right = self.eval_expr(&e.right)?;

    // value comparison
    if e.operator.token_type == TokenKind::EqEq {
      return Ok(Value::new(left == right));
    }

    if e.operator.token_type == TokenKind::ExEq {
      return Ok(Value::new(left != right));
    }

    // number arithmetic and comparison
    match e.operator.token_type {
      TokenKind::Plus => Ok(left + right),
      TokenKind::Minus => Ok(left - right),
      TokenKind::Asterisk => Ok(left * right),
      TokenKind::Slash => Ok(left / right),
      TokenKind::GreaterThan => Ok(Value::Bool(left > right)),
      TokenKind::GreaterEq => Ok(Value::Bool(left >= right)),
      TokenKind::LessThan => Ok(Value::Bool(left < right)),
      TokenKind::LessEq => Ok(Value::Bool(left <= right)),
      _ => Err(ScriptError {
        file: self.file.clone(),
        line: e.operator.line,
        msg: format!(
          "Invalid operator ({:?}) for {} and {}",
          e.operator, left, right
        ),
      }),
    }
  }
}

impl Visitor<IsExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &IsExpr) -> ExprEvalResult {
    let value = self.eval_expr(&e.value)?;

    Ok(Value::new(
      e.datatype.token_type
        == match value {
          Value::Nil => TokenKind::Nil,
          Value::Error(_) => TokenKind::Error,
          Value::Bool(_) => TokenKind::Bool,
          Value::Num(_) => TokenKind::Number,
          Value::Str(_) => TokenKind::String,
          Value::List(_) => TokenKind::List,
          Value::Callee(_) => TokenKind::Fn,
          Value::Class(_) => TokenKind::Class,
          _ => panic!("this should not be ever reached"),
        },
    ))
  }
}

impl Visitor<TernaryExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &TernaryExpr) -> ExprEvalResult {
    let result = self.eval_expr(&e.condition)?;

    if result.truthy() {
      self.eval_expr(&e.if_true)
    } else {
      self.eval_expr(&e.if_false)
    }
  }
}

impl Visitor<GroupingExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &GroupingExpr) -> ExprEvalResult {
    self.eval_expr(&e.expression)
  }
}

impl Visitor<LiteralExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &LiteralExpr) -> ExprEvalResult {
    Ok(e.value.clone())
  }
}

impl Visitor<UnaryExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &UnaryExpr) -> ExprEvalResult {
    let right = self.eval_expr(&e.right)?;

    match e.operator.token_type {
      TokenKind::Exclamation => Ok(!right),
      TokenKind::Minus => Ok(-right),
      TokenKind::Plus => {
        if let Value::Num(n) = right {
          Ok(Value::new(n.abs()))
        } else if let Value::Str(s) = right {
          match s.parse::<f64>() {
            Ok(n) => Ok(Value::new(n)),
            Err(err) => Ok(Value::new_err(format!(
              "error converting string to number: {}",
              err
            ))),
          }
        } else {
          Err(ScriptError {
            file: self.file.clone(),
            line: e.operator.line,
            msg: format!("invalid absolution on type {}", right),
          })
        }
      }
      _ => Err(ScriptError {
        file: self.file.clone(),
        line: e.operator.line,
        msg: format!("invalid unary operator {}", e.operator),
      }),
    }
  }
}

impl Visitor<VariableExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &VariableExpr) -> ExprEvalResult {
    self.lookup_variable(&e.name, e.id)
  }
}

impl Visitor<AssignExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &AssignExpr) -> ExprEvalResult {
    let value = self.eval_expr(&e.value)?;

    if let Some(depth) = self.locals.get(&e.id) {
      if let Err(msg) = self
        .env
        .assign_at(*depth, e.name.lexeme.clone(), value.clone())
      {
        return Err(ScriptError {
          file: self.file.clone(),
          line: e.name.line,
          msg: format!("assignment error: {}", msg),
        });
      }
    } else if let Err(msg) = self.env.assign(e.name.lexeme.clone(), value.clone()) {
      return Err(ScriptError {
        file: self.file.clone(),
        line: e.name.line,
        msg: format!("assignment error: {}", msg),
      });
    }
    Ok(value)
  }
}

impl Visitor<LogicalExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &LogicalExpr) -> ExprEvalResult {
    let left = self.eval_expr(&e.left)?;

    match e.operator.token_type {
      TokenKind::Or => {
        if left.truthy() {
          return Ok(left);
        }
      }
      TokenKind::And => {
        if !left.truthy() {
          return Ok(left);
        }
      }
      _ => {
        return Err(ScriptError {
          file: self.file.clone(),
          line: e.operator.line,
          msg: String::from("invalid attempt for logical comparison"),
        })
      }
    }

    self.eval_expr(&e.right)
  }
}

impl Visitor<SetExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &SetExpr) -> ExprEvalResult {
    let obj = self.eval_expr(&e.object)?;

    if let Value::Instance(mut instance) = obj {
      let value = self.eval_expr(&e.value)?;
      instance.members.define(&e.name.lexeme, value.clone());
      Ok(value)
    } else {
      Err(ScriptError {
        file: self.file.clone(),
        line: e.name.line,
        msg: String::from("only instances have properties"),
      })
    }
  }
}

impl Visitor<RangeExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, _: &RangeExpr) -> ExprEvalResult {
    //Err(ScriptError {
    //  file: self.file.clone(),
    //  line: e.token.line,
    //  msg: String::from("expected number with range expression"),
    //});

    panic!("unimplemented");
  }
}

impl Visitor<CallExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &CallExpr) -> ExprEvalResult {
    let callee = self.eval_expr(&e.callee)?;
    if let Value::Callee(func) = callee {
      let mut args = Vec::new();
      for arg in e.args.iter() {
        args.push(self.eval_expr(arg)?);
      }
      Ok(func.call(self, args, e.paren.line)?)
    } else if let Value::Class(class) = callee {
      let instance = Instance::new(
        class.name.clone(),
        class.instance_methods.snapshot(),
        EnvRef::default(),
      );
      Ok(Value::Instance(instance))
    } else {
      Err(ScriptError {
        file: self.file.clone(),
        line: e.paren.line,
        msg: format!("can't call type {}", callee),
      })
    }
  }
}

impl Visitor<GetExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &GetExpr) -> ExprEvalResult {
    match self.eval_expr(&e.object)? {
      Value::Instance(instance) => {
        self.last_object = Some(Value::Instance(Instance {
          instance_of: instance.instance_of,
          methods: instance.methods.snapshot(),
          members: instance.members.snapshot(),
        }));

        Ok(if let Some(v) = instance.methods.get(&e.name.lexeme) {
          v
        } else if let Some(v) = instance.members.get(&e.name.lexeme.clone()) {
          v
        } else {
          Value::Nil
        })
      }
      Value::Class(class) => {
        if let Some(v) = class.static_methods.get(&e.name.lexeme) {
          Ok(v)
        } else {
          Err(ScriptError {
            file: self.file.clone(),
            line: e.name.line,
            msg: format!(
              "static method {} not defined on class {}",
              e.name.lexeme, class.name
            ),
          })
        }
      }
      _ => Err(ScriptError {
        file: self.file.clone(),
        line: e.name.line,
        msg: String::from("only instances have properties"),
      }),
    }
  }
}

impl Visitor<ClosureExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &ClosureExpr) -> ExprEvalResult {
    Ok(Value::Callee(Function::new_closure(
      Rc::clone(&e.params),
      Rc::clone(&e.body),
      self.env.snapshot(),
    )))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::Interpreter;

  #[cfg(test)]
  mod parser {
    use super::*;
    #[cfg(test)]
    mod unit {
      use super::*;

      #[test]
      fn parser_should_increment_id_on_next_id() {
        let tokens = Vec::new();
        let mut p = Parser::new("test".into(), &tokens);
        assert_eq!(p.next_id(), 0);
        assert_eq!(p.current_id, 1);
      }

      #[test]
      fn test_parser_load_stmt() {
        const TEST_NAME: &str = "test_parser_load_stmt";
        const SCRIPT_SRC: &str = "load \"some_file.ss\";";
        let analysis = lex::analyze(TEST_NAME.into(), SCRIPT_SRC);
        assert!(analysis.is_ok());
        let analysis = analysis.unwrap();
        let res = parse(TEST_NAME.into(), &analysis.tokens);
        assert!(res.is_ok());
        let res = res.unwrap();

        assert_eq!(res.len(), 1);
        if let Stmt::Load(load) = res.first().unwrap() {
          assert_eq!(load.id, 1);
          if let Expr::Literal(e) = &load.path {
            assert_eq!(e.id, 0);
            if let Value::Str(s) = &e.value {
              assert_eq!(*s, String::from("some_file.ss"));
            } else {
              panic!("load path not string literal: {}", e.value);
            }
          } else {
            panic!("load path not literal value");
          }
        } else {
          panic!("load statement is not correct");
        }
      }
    }
  }
  #[cfg(test)]
  mod evaluator {
    use super::*;

    #[test]
    fn evaluation_of_is_should_result_in_expected_truth_values() {
      const SRC: &str = r#"
      fn is_nil(x) {
        assert(x is nil, true);
      }

      fn is_error(x) {
        assert(x is error, true);
      }

      fn is_bool(x) {
        assert(x is bool, true);
      }

      fn is_number(x) {
        assert(x is number, true);
      }

      fn is_string(x) {
        assert(x is string, true);
      }

      fn is_fn(x) {
        assert(x is fn, true);
      }

      fn is_class(x) {
        assert(x is class, true);
      }

      let x;

      x = nil;
      is_nil(x);
      is_nil(nil);

      x = true;
      is_bool(x);
      is_bool(false);

      x = 1.0;
      is_number(x);
      is_number(1);

      x = "test";
      is_string(x);
      is_string("another");

      x = is_fn;
      is_fn(x);
      is_fn(is_class);

      class Test {}

      x = Test;
      is_class(x);
      is_class(Test);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn evaluation_of_static_method_should_be_callable_from_class() {
      const SRC: &str = r#"
      class Test {
        fn @test() {
          return 100;
        }
      }

      assert(Test.test(), 100);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn evaluation_of_static_method_should_not_be_callable_from_class_when_method_does_not_begin_with_at(
    ) {
      const SRC: &str = r#"
      class Test {
        fn test() {
          return 100;
        }
      }

      assert(Test.test(), 100);
      "#;

      let i = Interpreter::new_with_test_support();
      assert!(matches!(i.exec(&"test".into(), SRC), Err(_)));
    }

    #[test]
    fn evaluation_should_pass_correct_variables_to_member_functions() {
      const SRC: &str = r#"
      let x = 100;
      class Test {
        fn test(self, x) {
          assert(x, 200);
        }
      }

      let test = Test();
      test.test(200);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn evaluation_should_not_allow_class_methods_if_they_do_not_start_with_an_at() {
      const SRC: &str = r#"
      class Test {
        fn test(self) {}
      }

      Test.test();
      "#;

      let i = Interpreter::new_with_test_support();
      assert!(i.exec(&"test".into(), SRC).is_err())
    }

    #[test]
    fn evaluation_should_allow_class_methods_only_if_they_start_with_an_at() {
      const SRC: &str = r#"
      class Test {
        fn @test() {}
      }

      Test.test();
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn evaluator_should_allow_functions_within_functions() {
      const SRC: &str = r#"
      fn outer() {
        fn inner() {
          return 1;
        }

        return inner;
      }

      let f = outer();
      assert(f(), 1);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }
  }
}
