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
  ($self:ident, $( $pattern:pat )|+ $( if $guard: expr )? $(,)?) => {
    {
    !$self.is_at_end() && matches!(&$self.peek().kind, $( $pattern )|+ $( if $guard )?)
    }
  };
}

// if the token type matches, advance
macro_rules! consume {
  ($self:ident, $types:pat, $alt:expr) => {{
    if check!($self, $types) {
      Ok($self.advance())
    } else {
      $self.make_err($alt)
    }
  }};
}

macro_rules! advance {
  ($self:ident) => {{
    if !$self.is_at_end() {
      $self.current += 1;
    }

    $self.previous()
  }};
}

macro_rules! advance_if_matches {
  ($self:ident, $( $pattern:pat )|+ $( if $guard: expr )? $(,)?) => {
    {
    if check!($self, $( $pattern )|+ $( if $guard )?) {
      advance!($self);
      true
    } else {
      false
    }
    }
  }
}

macro_rules! left_associative_binary {
  ($self:ident, $next:ident, $( $pattern:pat )|+ $( if $guard: expr )? $(,)?) => {
    {
    let mut expr = $self.$next()?;

    while advance_if_matches!($self, $( $pattern )|+ $( if $guard )?) {
      let op = $self.previous();
      let right = $self.$next()?;
      expr = Expr::new_logical(Box::new(expr), op, Box::new(right), $self.next_id());
    }

    Ok(expr)
    }
  }
}

macro_rules! right_associative_unary {
  ($self:ident, $next:ident, $( $pattern:pat )|+ $( if $guard: expr )? $(,)?) => {{
    if advance_if_matches!($self, $( $pattern )|+ $( if $guard )?) {
      let op = $self.previous();
      let right = $self.unary()?;
      Ok(Expr::new_unary(op, Box::new(right), $self.next_id()))
    } else {
      $self.$next()
    }
  }};
}

pub fn parse(file: OsString, tokens: &[Token]) -> ParseResult {
  let mut parser = Parser::new(file, tokens);
  parser.parse()
}

struct Parser<'tokens> {
  file: OsString,
  tokens: &'tokens [Token],
  current: usize,
  current_id: usize,
}

impl<'tokens> Parser<'tokens> {
  fn new(file: OsString, tokens: &'tokens [Token]) -> Self {
    Self {
      file,
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
    if advance_if_matches!(self, TokenKind::Let) {
      self.var_decl()
    } else if advance_if_matches!(self, TokenKind::Fn) {
      self.fn_decl("function")
    } else if advance_if_matches!(self, TokenKind::Class) {
      self.class_decl()
    } else if advance_if_matches!(self, TokenKind::Print) {
      self.print_statement()
    } else if advance_if_matches!(self, TokenKind::Return) {
      self.return_statement()
    } else if advance_if_matches!(self, TokenKind::For) {
      self.for_statement()
    } else if advance_if_matches!(self, TokenKind::While) {
      self.while_statement()
    } else if advance_if_matches!(self, TokenKind::If) {
      self.if_statement()
    } else if advance_if_matches!(self, TokenKind::LeftBrace) {
      Ok(Stmt::new_block(self.block()?, self.next_id()))
    } else if advance_if_matches!(self, TokenKind::Load) {
      self.load_statement()
    } else if advance_if_matches!(self, TokenKind::Loadr) {
      self.loadr_statement()
    } else {
      self.expr_statement()
    }
  }

  fn var_decl(&mut self) -> StatementResult {
    let name = consume!(self, TokenKind::Identifier(_), "expected variable name")?;
    let mut expr = None;
    if advance_if_matches!(self, TokenKind::Equal) {
      expr = Some(self.expression()?);
    }

    consume!(
      self,
      TokenKind::Semicolon,
      "expected ';' after variable decl"
    )?;
    Ok(Stmt::new_let(name, expr, self.next_id()))
  }

  fn fn_decl(&mut self, kind: &str) -> StatementResult {
    let name = consume!(
      self,
      TokenKind::Identifier(_),
      &format!("expect {} name", kind)
    )?;
    consume!(
      self,
      TokenKind::LeftParen,
      &format!("expect '(' after {} name", kind)
    )?;
    let mut params = Vec::new();
    if check!(self, TokenKind::RightParen) {
      loop {
        params.push(consume!(
          self,
          TokenKind::Identifier(_),
          "expected identifier"
        )?);

        if !advance_if_matches!(self, TokenKind::Comma) {
          break;
        }
      }
    }

    consume!(self, TokenKind::RightParen, "expect ')' after parameters")?;
    consume!(
      self,
      TokenKind::LeftBrace,
      &format!("Expect '{{' before {} body", kind)
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
    let name = consume!(self, TokenKind::Identifier(_), "expect class name")?;
    consume!(self, TokenKind::LeftBrace, "expect '{' before class body")?;

    let mut methods = Vec::new();
    while !check!(self, TokenKind::RightBrace) {
      if advance_if_matches!(self, TokenKind::Fn) {
        methods.push(self.fn_decl("method")?);
      } else {
        return Err(ScriptError {
          file: self.file.clone().clone(),
          line: name.line,
          msg: String::from("invalid token found"),
        });
      }
    }

    consume!(self, TokenKind::RightBrace, "expect '}' after class body")?;

    Ok(Stmt::new_class(name, methods, self.next_id()))
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    consume!(self, TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_print(expr, self.next_id()))
  }

  fn return_statement(&mut self) -> StatementResult {
    let keyword = self.previous();
    let mut value = None;

    if !check!(self, TokenKind::Semicolon) {
      value = Some(self.expression()?);
    }

    consume!(
      self,
      TokenKind::Semicolon,
      "expected ';' after return value"
    )?;
    Ok(Stmt::new_return(keyword, value, self.next_id()))
  }

  fn for_statement(&mut self) -> StatementResult {
    let token = self.previous();

    let initializer = if advance_if_matches!(self, TokenKind::Let) {
      self.var_decl()?
    } else {
      self.expr_statement()?
    };

    let condition = self.expression()?;
    consume!(self, TokenKind::Semicolon, "Expect ';' after condition")?;

    let increment = self.expression()?;
    consume!(self, TokenKind::LeftBrace, "Expect '{' after increment")?;

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
    consume!(self, TokenKind::LeftBrace, "missing '{' after if condition")?;
    let body = self.block()?;
    Ok(Stmt::new_while(token, condition, body, self.next_id()))
  }

  fn if_statement(&mut self) -> StatementResult {
    let condition = self.expression()?;
    consume!(self, TokenKind::LeftBrace, "missing '{' after if condition")?;
    let if_true = self.block()?;
    let mut if_false = None;

    if advance_if_matches!(self, TokenKind::Else) {
      if_false = if advance_if_matches!(self, TokenKind::LeftBrace) {
        Some(Box::new(Stmt::new_block(self.block()?, self.next_id())))
      } else if advance_if_matches!(self, TokenKind::If) {
        Some(Box::new(self.if_statement()?))
      } else {
        return Err(ScriptError {
          file: self.file.clone().clone(),
          line: self.peek().line,
          msg: format!("invalid token after token {}", self.peek()),
        });
      };
    }

    Ok(Stmt::new_if(condition, if_true, if_false, self.next_id()))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    consume!(self, TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_expression(expr, self.next_id()))
  }

  fn load_statement(&mut self) -> StatementResult {
    let load = self.previous();
    let file = self.expression()?;
    consume!(self, TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_load(load, file, self.next_id()))
  }

  fn loadr_statement(&mut self) -> StatementResult {
    let loadr = self.previous();
    let file = self.expression()?;
    consume!(self, TokenKind::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_loadr(loadr, file, self.next_id()))
  }

  fn expression(&mut self) -> ExprResult {
    self.is()
  }

  fn is(&mut self) -> ExprResult {
    let mut expr = self.ternary()?;

    if advance_if_matches!(self, TokenKind::Is) {
      let token = self.previous();
      if advance_if_matches!(
        self,
        TokenKind::Nil
          | TokenKind::Error
          | TokenKind::Bool
          | TokenKind::Number
          | TokenKind::String
          | TokenKind::List
          | TokenKind::Fn
          | TokenKind::Class
      ) {
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

    if advance_if_matches!(self, TokenKind::Conditional) {
      let if_true = self.expression()?;
      consume!(
        self,
        TokenKind::Colon,
        "expected ':' for conditional operator"
      )?;
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

    if advance_if_matches!(self, TokenKind::Equal) {
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
          file: self.file.clone().clone(),
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

    if advance_if_matches!(self, TokenKind::Range) {
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
    left_associative_binary!(self, comparison, TokenKind::ExEq | TokenKind::EqEq)
  }

  fn comparison(&mut self) -> ExprResult {
    left_associative_binary!(
      self,
      addition,
      TokenKind::GreaterThan | TokenKind::GreaterEq | TokenKind::LessThan | TokenKind::LessEq
    )
  }

  fn addition(&mut self) -> ExprResult {
    left_associative_binary!(self, multiplication, TokenKind::Plus | TokenKind::Minus)
  }

  fn multiplication(&mut self) -> ExprResult {
    left_associative_binary!(self, unary, TokenKind::Slash | TokenKind::Asterisk)
  }

  fn unary(&mut self) -> ExprResult {
    right_associative_unary!(
      self,
      call,
      TokenKind::Exclamation | TokenKind::Minus | TokenKind::Plus
    )
  }

  fn call(&mut self) -> ExprResult {
    let mut expr = self.primary()?;

    loop {
      if advance_if_matches!(self, TokenKind::LeftParen) {
        expr = self.finish_call(expr)?;
      } else if advance_if_matches!(self, TokenKind::Dot) {
        let name = consume!(
          self,
          TokenKind::Identifier(_),
          "expected property name after '.'"
        )?;
        expr = Expr::Get(GetExpr::new(Box::new(expr), name, self.next_id()));
      } else {
        break;
      }
    }

    Ok(expr)
  }

  fn primary(&mut self) -> ExprResult {
    if !self.is_at_end() {
      if let Some(value) = match &self.peek().kind {
        TokenKind::Nil => Some(Value::Nil),
        TokenKind::False => Some(Value::new(false)),
        TokenKind::True => Some(Value::new(true)),
        TokenKind::NumberLiteral(n) => Some(Value::new(*n)),
        TokenKind::StringLiteral(s) => Some(Value::new(s.clone())),
        _ => None,
      } {
        advance!(self);
        let prev = self.previous();
        return Ok(Expr::new_literal(value, self.next_id()));
      }
    }

    if advance_if_matches!(self, TokenKind::Identifier(_)) {
      return Ok(Expr::new_variable(self.previous(), self.next_id()));
    }

    if advance_if_matches!(self, TokenKind::LeftParen) {
      let expr = self.expression()?;
      consume!(self, TokenKind::RightParen, "Expect ')' after expression.")?;
      return Ok(Expr::new_grouping(Box::new(expr), self.next_id()));
    }

    if advance_if_matches!(self, TokenKind::Pipe) {
      let mut params = Vec::new();
      if !check!(self, TokenKind::Pipe) {
        loop {
          params.push(consume!(
            self,
            TokenKind::Identifier(_),
            "expected identifier"
          )?);

          if !advance_if_matches!(self, TokenKind::Comma) {
            break;
          }
        }
      }

      consume!(self, TokenKind::Pipe, "expect '|' after closure parameters")?;
      consume!(
        self,
        TokenKind::LeftBrace,
        "expect '{{' before closure body"
      )?;

      let body = self.block()?;

      return Ok(Expr::new_closure(
        Rc::new(params),
        Rc::new(body),
        self.next_id(),
      ));
    }

    Err(ScriptError {
      file: self.file.clone().clone(),
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

    while advance_if_matches!(self, types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_logical(Box::new(expr), op, Box::new(right), self.next_id());
    }

    Ok(expr)
  }

  fn block(&mut self) -> Result<Vec<Stmt>, ScriptError> {
    let mut v = Vec::new();

    while !check!(self, TokenKind::RightBrace) {
      v.push(self.decl()?);
    }

    consume!(self, TokenKind::RightBrace, "Expect '}' after block")?;

    Ok(v)
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
      if self.previous().kind == TokenKind::Semicolon {
        return;
      }

      match self.peek().kind {
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

    if !check!(self, TokenKind::RightParen) {
      loop {
        args.push(self.expression()?);

        if !advance_if_matches!(self, TokenKind::Comma) {
          break;
        }
      }
    }

    let paren = consume!(self, TokenKind::RightParen, "Expect ')' after arguments")?;

    Ok(Expr::new_call(
      Box::new(callee),
      paren,
      args,
      self.next_id(),
    ))
  }

  fn make_err(&self, msg: &str) -> Result<Token, ScriptError> {
    Err(ScriptError {
      file: self.file.clone().clone(),
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
    if let TokenKind::Identifier(ident) = &name.kind {
      match if let Some(depth) = self.locals.get(&id) {
        self.env.lookup_at(*depth, ident)
      } else {
        self.env.lookup(ident)
      } {
        Some(v) => Ok(v),
        None => Err(ScriptError {
          file: self.file.clone().clone(),
          line: name.line,
          msg: format!("used uninitialized variable '{}'", ident),
        }),
      }
    } else {
      return Err(ScriptError {
        file: self.file.clone().clone(),
        line: name.line,
        msg: format!("tried to lookup non variable"),
      });
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

    if let TokenKind::Identifier(ident) = &e.name.kind {
      self.env.define(ident, value);
    } else {
      return Err(ScriptError {
        file: self.file.clone().clone(),
        line: e.name.line,
        msg: format!("tried to instantiate an invalid variable name"),
      });
    }

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
    if let TokenKind::Identifier(ident) = &s.name.kind {
      self.env.define(ident, Value::Nil);

      let mut static_methods = EnvRef::new_with_enclosing(self.env.snapshot());
      let mut instance_methods = EnvRef::new_with_enclosing(self.env.snapshot());

      // for every method in the class body
      for method in s.methods.iter() {
        // pull out the function from the method
        if let Stmt::Function(f) = method {
          if let TokenKind::Identifier(func_name) = &f.name.kind {
            // if it begins with an '@' it is static
            if func_name.starts_with('@') {
              // function names must be longer than one character
              if func_name.len() == 1 {
                return Err(ScriptError {
                  file: self.file.clone().clone(),
                  line: f.name.line,
                  msg: format!(
                    "class method {} for class {} cannot be one char long if its only char is '@'",
                    func_name, ident,
                  ),
                });
              }
              // strip off the '@'
              let name = &func_name[1..];
              let func = Function::new_script(
                String::from(name),
                Rc::clone(&f.params),
                Rc::clone(&f.body),
                static_methods.snapshot(),
              );

              if static_methods.define(name, Value::new(func)) {
                return Err(ScriptError {
                  file: self.file.clone().clone(),
                  line: f.name.line,
                  msg: format!("redeclaration of class method {}", func_name),
                });
              }
            } else {
              let name = &func_name[..];
              let func = Function::new_method(
                func_name.clone(),
                Rc::clone(&f.params),
                Rc::clone(&f.body),
                instance_methods.snapshot(),
              );
              if instance_methods.define(name, Value::new(func)) {
                return Err(ScriptError {
                  file: self.file.clone().clone(),
                  line: f.name.line,
                  msg: format!("redeclaration of class method {}", func_name),
                });
              }
            };
          }
        } else {
          return Err(ScriptError {
            file: self.file.clone().clone(),
            line: s.name.line,
            msg: format!("invalid declaration inside <class {}> body", ident),
          });
        }
      }

      let class = Value::Class(Class::new(ident.clone(), static_methods, instance_methods));

      self
        .env
        .assign(ident.clone(), class)
        .map_err(|err| ScriptError {
          file: self.file.clone().clone(),
          line: s.name.line,
          msg: format!("error assigning class: {}", err),
        })?;

      Ok(StatementType::Regular(Value::Nil))
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
        line: s.name.line,
        msg: format!(""),
      })
    }
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
    if let TokenKind::Identifier(ident) = &e.name.kind {
      let func = Function::new_script(
        ident.clone(),
        Rc::clone(&e.params),
        Rc::clone(&e.body),
        self.env.snapshot(),
      );
      self.env.define(ident, Value::Callee(func));
      Ok(StatementType::Regular(Value::Nil))
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
        line: e.name.line,
        msg: format!(
          "tried to declare a function with a non-identifier: {}",
          e.name
        ),
      })
    }
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
          file: self.file.clone().clone(),
          line: s.load.line,
          msg: format!("failed loading file {}: {}", path, err),
        }),
      }
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
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
        file: self.file.clone().clone(),
        line: s.loadr.line,
        msg: "unable to check current working directory".to_string(),
      })?;
      let mut fp = PathBuf::from(&self.file.clone().clone());
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
          file: self.file.clone().clone(),
          line: s.loadr.line,
          msg: format!("failed loading file {}: {}", wd.as_path().display(), err),
        }),
      }
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
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
    if e.operator.kind == TokenKind::EqEq {
      return Ok(Value::new(left == right));
    }

    if e.operator.kind == TokenKind::ExEq {
      return Ok(Value::new(left != right));
    }

    // number arithmetic and comparison
    match e.operator.kind {
      TokenKind::Plus => Ok(left + right),
      TokenKind::Minus => Ok(left - right),
      TokenKind::Asterisk => Ok(left * right),
      TokenKind::Slash => Ok(left / right),
      TokenKind::GreaterThan => Ok(Value::Bool(left > right)),
      TokenKind::GreaterEq => Ok(Value::Bool(left >= right)),
      TokenKind::LessThan => Ok(Value::Bool(left < right)),
      TokenKind::LessEq => Ok(Value::Bool(left <= right)),
      _ => Err(ScriptError {
        file: self.file.clone().clone(),
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
      e.datatype.kind
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

    match e.operator.kind {
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
            file: self.file.clone().clone(),
            line: e.operator.line,
            msg: format!("invalid absolution on type {}", right),
          })
        }
      }
      _ => Err(ScriptError {
        file: self.file.clone().clone(),
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
    if let TokenKind::Identifier(ident) = &e.name.kind {
      let value = self.eval_expr(&e.value)?;
      if let Some(depth) = self.locals.get(&e.id) {
        if let Err(msg) = self.env.assign_at(*depth, ident.clone(), value.clone()) {
          return Err(ScriptError {
            file: self.file.clone().clone(),
            line: e.name.line,
            msg: format!("assignment error: {}", msg),
          });
        }
      } else if let Err(msg) = self.env.assign(ident.clone(), value.clone()) {
        return Err(ScriptError {
          file: self.file.clone().clone(),
          line: e.name.line,
          msg: format!("assignment error: {}", msg),
        });
      }
      Ok(value)
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
        line: e.name.line,
        msg: format!("tried to assign to non-identifier"),
      })
    }
  }
}

impl Visitor<LogicalExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &LogicalExpr) -> ExprEvalResult {
    let left = self.eval_expr(&e.left)?;

    match e.operator.kind {
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
          file: self.file.clone().clone(),
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
    if let TokenKind::Identifier(ident) = &e.name.kind {
      let obj = self.eval_expr(&e.object)?;
      if let Value::Instance(mut instance) = obj {
        let value = self.eval_expr(&e.value)?;
        instance.members.define(ident, value.clone());
        Ok(value)
      } else {
        Err(ScriptError {
          file: self.file.clone().clone(),
          line: e.name.line,
          msg: String::from("only instances have properties"),
        })
      }
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
        line: e.name.line,
        msg: String::from("tried to assign to non-identifier"),
      })
    }
  }
}

impl Visitor<RangeExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, _: &RangeExpr) -> ExprEvalResult {
    //Err(ScriptError {
    //  file: self.file.clone().clone().clone(),
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
        file: self.file.clone().clone(),
        line: e.paren.line,
        msg: format!("can't call type {}", callee),
      })
    }
  }
}

impl Visitor<GetExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &GetExpr) -> ExprEvalResult {
    if let TokenKind::Identifier(ident) = &e.name.kind {
      match self.eval_expr(&e.object)? {
        Value::Instance(instance) => {
          self.last_object = Some(Value::Instance(Instance {
            instance_of: instance.instance_of,
            methods: instance.methods.snapshot(),
            members: instance.members.snapshot(),
          }));

          Ok(if let Some(v) = instance.methods.get(ident) {
            v
          } else if let Some(v) = instance.members.get(ident) {
            v
          } else {
            Value::Nil
          })
        }
        Value::Class(class) => {
          if let Some(v) = class.static_methods.get(ident) {
            Ok(v)
          } else {
            Err(ScriptError {
              file: self.file.clone().clone(),
              line: e.name.line,
              msg: format!(
                "static method {} not defined on class {}",
                ident, class.name
              ),
            })
          }
        }
        _ => Err(ScriptError {
          file: self.file.clone().clone(),
          line: e.name.line,
          msg: String::from("only instances have properties"),
        }),
      }
    } else {
      Err(ScriptError {
        file: self.file.clone().clone(),
        line: e.name.line,
        msg: String::from("tried to read from non-identifier"),
      })
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
        let analysis = lex::analyze("test".into(), SCRIPT_SRC);
        assert!(analysis.is_ok());
        let analysis = analysis.unwrap();
        let res = parse("test".into(), &analysis.tokens);
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
