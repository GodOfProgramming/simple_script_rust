use crate::env::EnvRef;
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GroupingExpr, LiteralExpr,
  LogicalExpr, RangeExpr, TernaryExpr, UnaryExpr, VariableExpr,
};
use crate::lex::{self, Token, TokenType};
use crate::res;
use crate::stmt::{
  self, BlockStmt, ExpressionStmt, FunctionStmt, IfStmt, LoadStmt, LoadrStmt, PrintStmt,
  ReturnStmt, Stmt, VarStmt, WhileStmt,
};
use crate::types::{Function, Value, Values, Visitor};
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

pub fn parse(file: OsString, tokens: &[Token]) -> ParseResult {
  let mut parser = Parser::new(file, tokens);
  parser.parse()
}

struct Parser<'tokens> {
  file: OsString,
  tokens: &'tokens [Token],
  current: usize,
  statement_id: usize,
  expression_id: usize,
}

impl<'tokens> Parser<'tokens> {
  fn new(file: OsString, tokens: &'tokens [Token]) -> Self {
    Self {
      file,
      tokens,
      current: 0,
      statement_id: 0,
      expression_id: 0,
    }
  }

  fn next_stmt_id(&mut self) -> usize {
    let id = self.statement_id;
    self.statement_id += 1;
    id
  }

  fn next_expr_id(&mut self) -> usize {
    let id = self.expression_id;
    self.expression_id += 1;
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
    match if self.match_token(&[TokenType::Let]) {
      self.var_decl()
    } else if self.match_token(&[TokenType::Fn]) {
      self.fn_decl("function")
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
    if self.match_token(&[TokenType::Equal]) {
      expr = Some(self.expression()?);
    }

    self.consume(TokenType::Semicolon, "expected ';' after variable decl")?;
    Ok(Stmt::new_var(name, expr, self.next_stmt_id()))
  }

  fn fn_decl(&mut self, kind: &str) -> StatementResult {
    let name = self.consume(TokenType::Identifier, &format!("expect {} name", kind))?;
    self.consume(
      TokenType::LeftParen,
      &format!("expect '(' after {} name", kind),
    )?;
    let mut params = Vec::new();
    if !self.check(TokenType::RightParen) {
      loop {
        params.push(self.consume(TokenType::Identifier, "expected identifier")?);

        if !self.match_token(&[TokenType::Comma]) {
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

    Ok(Stmt::new_function(
      name,
      Rc::new(params),
      Rc::new(body),
      self.next_stmt_id(),
    ))
  }

  fn statement(&mut self) -> StatementResult {
    if self.match_token(&[TokenType::Print]) {
      self.print_statement()
    } else if self.match_token(&[TokenType::Return]) {
      self.return_statement()
    } else if self.match_token(&[TokenType::For]) {
      self.for_statement()
    } else if self.match_token(&[TokenType::While]) {
      self.while_statement()
    } else if self.match_token(&[TokenType::If]) {
      self.if_statement()
    } else if self.match_token(&[TokenType::LeftBrace]) {
      Ok(Stmt::new_block(self.block()?, self.next_stmt_id()))
    } else if self.match_token(&[TokenType::Load]) {
      self.load_statement()
    } else if self.match_token(&[TokenType::Loadr]) {
      self.loadr_statement()
    } else {
      self.expr_statement()
    }
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_print(expr, self.next_stmt_id()))
  }

  fn return_statement(&mut self) -> StatementResult {
    let keyword = self.previous();
    let mut value = None;
    if !self.check(TokenType::Semicolon) {
      value = Some(self.expression()?);
    }

    self.consume(TokenType::Semicolon, "expected ';' after return value")?;
    Ok(Stmt::new_return(keyword, value, self.next_stmt_id()))
  }

  fn for_statement(&mut self) -> StatementResult {
    let token = self.previous();

    let initializer = if self.match_token(&[TokenType::Let]) {
      self.var_decl()?
    } else {
      self.expr_statement()?
    };

    let condition = self.expression()?;
    self.consume(TokenType::Semicolon, "Expect ';' after condition")?;

    let increment = self.expression()?;
    self.consume(TokenType::LeftBrace, "Expect '{' after increment")?;

    let body = Stmt::new_block(
      vec![
        initializer,
        Stmt::new_while(
          token,
          condition,
          vec![
            Stmt::new_block(self.block()?, self.next_stmt_id()),
            Stmt::new_expression(increment, self.next_stmt_id()),
          ],
          self.next_stmt_id(),
        ),
      ],
      self.next_stmt_id(),
    );

    Ok(body)
  }

  fn while_statement(&mut self) -> StatementResult {
    let token = self.previous();
    let condition = self.expression()?;
    self.consume(TokenType::LeftBrace, "missing '{' after if condition")?;
    let body = self.block()?;
    Ok(Stmt::new_while(token, condition, body, self.next_stmt_id()))
  }

  fn if_statement(&mut self) -> StatementResult {
    let condition = self.expression()?;
    self.consume(TokenType::LeftBrace, "missing '{' after if condition")?;
    let if_true = self.block()?;
    let mut if_false = None;

    if self.match_token(&[TokenType::Else]) {
      if_false = if self.match_token(&[TokenType::LeftBrace]) {
        Some(Box::new(Stmt::new_block(
          self.block()?,
          self.next_stmt_id(),
        )))
      } else if self.match_token(&[TokenType::If]) {
        Some(Box::new(self.if_statement()?))
      } else {
        return Err(ScriptError {
          file: self.file.clone(),
          line: self.peek().line,
          msg: format!("invalid token after token {}", self.peek()),
        });
      };
    }

    Ok(Stmt::new_if(
      condition,
      if_true,
      if_false,
      self.next_stmt_id(),
    ))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_expression(expr, self.next_stmt_id()))
  }

  fn load_statement(&mut self) -> StatementResult {
    let load = self.previous();
    let file = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_load(load, file, self.next_stmt_id()))
  }

  fn loadr_statement(&mut self) -> StatementResult {
    let loadr = self.previous();
    let file = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_loadr(loadr, file, self.next_stmt_id()))
  }

  fn expression(&mut self) -> ExprResult {
    self.ternary()
  }

  // TODO
  fn _list(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.match_token(&[TokenType::Comma]) {
      let op = self.previous();
      let right = self._list()?;
      expr = Expr::new_binary(Box::new(expr), op, Box::new(right), self.next_expr_id());
    }

    Ok(expr)
  }

  fn ternary(&mut self) -> ExprResult {
    let mut expr = self.assignment()?;

    if self.match_token(&[TokenType::Conditional]) {
      let if_true = self.expression()?;
      self.consume(TokenType::Colon, "expected ':' for conditional operator")?;
      let if_false = self.expression()?;
      expr = Expr::new_ternary(
        Box::new(expr),
        Box::new(if_true),
        Box::new(if_false),
        self.next_expr_id(),
      );
    }

    Ok(expr)
  }

  fn assignment(&mut self) -> ExprResult {
    let expr = self.range()?;

    if self.match_token(&[TokenType::Equal]) {
      let equals = self.previous();
      let value = self.assignment()?;

      if let Expr::Variable(v) = expr {
        Ok(Expr::new_assign(
          v.name,
          Box::new(value),
          self.next_expr_id(),
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

    if self.match_token(&[TokenType::Range]) {
      let token = self.previous();
      let end = self.or()?;
      begin = Expr::new_range(Box::new(begin), token, Box::new(end), self.next_expr_id());
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
    self.right_associative_unary(
      Parser::call,
      &[TokenType::Exclamation, TokenType::Minus, TokenType::Plus],
    )
  }

  fn call(&mut self) -> ExprResult {
    let mut expr = self.primary()?;

    while self.match_token(&[TokenType::LeftParen]) {
      expr = self.finish_call(expr)?;
    }

    Ok(expr)
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
        return Ok(Expr::new_literal(Value::from(v), self.next_expr_id()));
      }
    }

    if self.match_token(&[TokenType::Identifier]) {
      return Ok(Expr::new_variable(self.previous(), self.next_expr_id()));
    }

    if self.match_token(&[TokenType::LeftParen]) {
      let expr = self.expression()?;
      self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
      return Ok(Expr::new_grouping(Box::new(expr), self.next_expr_id()));
    }

    if self.match_token(&[TokenType::Pipe]) {
      let mut params = Vec::new();
      if !self.check(TokenType::Pipe) {
        loop {
          params.push(self.consume(TokenType::Identifier, "expected identifier")?);

          if !self.match_token(&[TokenType::Comma]) {
            break;
          }
        }
      }

      self.consume(TokenType::Pipe, "expect '|' after closure parameters")?;

      self.consume(TokenType::LeftBrace, "expect '{{' before closure body")?;

      let body = self.block()?;

      return Ok(Expr::new_closure(
        Rc::new(params),
        Rc::new(body),
        self.next_expr_id(),
      ));
    }

    // TODO proper error handling

    Err(ScriptError {
      file: self.file.clone(),
      line: self.peek().line,
      msg: String::from("could not find valid primary token"),
    })
  }

  fn left_associative_logical(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenType],
  ) -> ExprResult {
    let mut expr = next(self)?;

    // TODO stop cloning
    while self.match_token(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_logical(Box::new(expr), op, Box::new(right), self.next_expr_id());
    }

    Ok(expr)
  }

  fn left_associative_binary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenType],
  ) -> ExprResult {
    let mut expr = next(self)?;

    // TODO stop cloning
    while self.match_token(types) {
      let op = self.previous();
      let right = next(self)?;
      expr = Expr::new_binary(Box::new(expr), op, Box::new(right), self.next_expr_id());
    }

    Ok(expr)
  }

  fn right_associative_unary(
    &mut self,
    next: fn(&mut Self) -> ExprResult,
    types: &[TokenType],
  ) -> ExprResult {
    if self.match_token(types) {
      let op = self.previous();
      let right = self.unary()?;
      Ok(Expr::new_unary(op, Box::new(right), self.next_expr_id()))
    } else {
      next(self)
    }
  }

  fn block(&mut self) -> Result<Vec<Stmt>, ScriptError> {
    let mut v = Vec::new();

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      v.push(self.decl()?);
    }

    self.consume(TokenType::RightBrace, "Expect '}' after block")?;

    Ok(v)
  }

  fn match_token(&mut self, types: &[TokenType]) -> bool {
    for token_type in types.iter() {
      if self.check(*token_type) {
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

  fn peek(&self) -> &'tokens Token {
    &self.tokens[self.current]
  }

  fn previous(&self) -> Token {
    self.tokens[self.current - 1].clone()
  }

  fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<Token, ScriptError> {
    if self.check(token_type) {
      Ok(self.advance())
    } else {
      Err(ScriptError {
        file: self.file.clone(),
        line: self.peek().line,
        msg: String::from(msg),
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
        TokenType::Fn => return,
        TokenType::Let => return,
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

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments")?;

    Ok(Expr::new_call(
      Box::new(callee),
      paren,
      args,
      self.next_expr_id(),
    ))
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
  locals: HashMap<usize, usize>,
}

impl Evaluator {
  fn new(file: OsString, env: EnvRef) -> Self {
    Self {
      file,
      env,
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
      println!("looking up {} at depth {}", name.lexeme, depth);
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

impl Visitor<VarStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &VarStmt) -> StmtEvalResult {
    let mut value = Value::Nil;

    if let Some(i) = &e.initializer {
      value = self.eval_expr(i)?;
    }

    self.env.define(e.name.lexeme.clone(), value);

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

impl Visitor<IfStmt, StmtEvalResult> for Evaluator {
  fn visit(&mut self, e: &IfStmt) -> StmtEvalResult {
    let result = self.eval_expr(&e.condition)?;
    if self.is_truthy(&result) {
      self.eval_block(&e.if_true, self.env.snapshot())
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
      if !self.is_truthy(&res) {
        break;
      }
      match self.eval_block(&e.body, self.env.snapshot())? {
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
    let name = e.name.lexeme.clone();
    let func = Function::new_script(
      e.name.lexeme.clone(),
      Rc::clone(&e.params),
      Rc::clone(&e.body),
    );
    self.env.define(name, Value::Callee(func));
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
          _ => Err(ScriptError {
            file: self.file.clone(),
            line: e.operator.line,
            msg: format!("Invalid operator ({:?}) for {} and {}", e.operator, l, r),
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

    Err(ScriptError {
      file: self.file.clone(),
      line: e.operator.line,
      msg: format!(
        "Combination of {:?} and {:?} not allowed with {:?}",
        left, right, e.operator
      ),
    })
  }
}

impl Visitor<TernaryExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &TernaryExpr) -> ExprEvalResult {
    let result = self.eval_expr(&e.condition)?;

    if self.is_truthy(&result) {
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
      TokenType::Exclamation => Ok(Value::Bool(!self.is_truthy(&right))),
      TokenType::Minus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(-n))
        } else {
          Err(ScriptError {
            file: self.file.clone(),
            line: e.operator.line,
            msg: format!("invalid negation on type {}", right),
          })
        }
      }
      TokenType::Plus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(n.abs()))
        } else if let Value::Str(s) = right {
          match s.parse() {
            Ok(n) => Ok(Value::Num(n)),
            Err(err) => Err(ScriptError {
              file: self.file.clone(),
              line: e.operator.line,
              msg: format!("string parse error: {}", err),
            }),
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

impl Visitor<RangeExpr, ExprEvalResult> for Evaluator {
  fn visit(&mut self, e: &RangeExpr) -> ExprEvalResult {
    let begin = self.eval_expr(&e.begin)?;
    let end = self.eval_expr(&e.end)?;

    if let Value::Num(begin) = begin {
      if let Value::Num(end) = end {
        return Ok(Value::List(Values::new(
          ((begin.round() as i64)..(end.round() as i64))
            .map(|n| Value::Num(n as f64))
            .collect(),
        )));
      }
    }

    Err(ScriptError {
      file: self.file.clone(),
      line: e.token.line,
      msg: String::from("expected number with range expression"),
    })
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
    } else {
      Err(ScriptError {
        file: self.file.clone(),
        line: e.paren.line,
        msg: format!("can't call type {}", callee),
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

#[cfg(Test)]
mod tests {
  use super::*;

  #[test]
  fn test_parser_load_stmt() {
    const test_name: &str = "test_parser_load_stmt";
    const script: &str = "load \"some_file.ss\"";
    let analysis = lex::analyze(test_name, script).unwrap();
    let res = parse(test_name, analysis).unwrap();

    assert_eq!(res.len(), 1);
    if let Stmt::Load(load) = res.first() {
      assert_eq!(load.id, 1);
      if let Expr::Literal(e) = load.path {
        assert_eq!(e.id, 1);
        if let Value::Str(s) = e.value {
          assert_eq!(s, String::from("some_file.ss"));
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
