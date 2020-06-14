use crate::env::{Env, EnvRef};
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GroupingExpr, LiteralExpr,
  LogicalExpr, RangeExpr, TernaryExpr, UnaryExpr, VariableExpr, Visitor as ExprVisitor,
};
use crate::lex::{self, LexicalErr, Token, TokenType};
use crate::stmt::{
  self, BlockStmt, ExpressionStmt, FunctionStmt, IfStmt, LoadStmt, PrintStmt, ReturnStmt, Stmt,
  VarStmt, Visitor as StmtVisitor, WhileStmt,
};
use crate::types::{CallErr, Closure, ScriptFunction, Value, Values};
use std::cell::RefCell;
use std::fmt::{self, Display};
use std::fs;
use std::rc::Rc;

pub struct AstErr {
  pub file: String,
  pub line: usize,
  pub msg: String,
}

impl Display for AstErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} ({}): {}", self.file, self.line, self.msg)
  }
}

impl From<LexicalErr> for AstErr {
  fn from(err: LexicalErr) -> Self {
    Self {
      file: err.file,
      line: err.line,
      msg: err.msg,
    }
  }
}

impl From<CallErr> for AstErr {
  fn from(err: CallErr) -> Self {
    Self {
      file: err.file,
      line: err.line,
      msg: err.msg,
    }
  }
}

type ParseResult = Result<Vec<Stmt>, AstErr>;
type StatementResult = Result<Stmt, AstErr>;
type ExprResult = Result<Expr, AstErr>;

pub fn parse<'a, 'b>(file: &'a str, tokens: &'b Vec<Token>) -> ParseResult {
  let mut parser = Parser::new(file, tokens);
  parser.parse()
}

struct Parser<'a, 'b> {
  file: &'a str,
  tokens: &'b Vec<Token>,
  current: usize,
}

impl<'a, 'b> Parser<'a, 'b> {
  fn new(file: &'a str, tokens: &'b Vec<Token>) -> Self {
    Self {
      file,
      tokens,
      current: 0,
    }
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
    } else if self.match_token(vec![TokenType::Fn]) {
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
      expr = Some(self.expression()?);
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
    } else if self.match_token(vec![TokenType::Load]) {
      self.load_statement()
    } else {
      self.expr_statement()
    }
  }

  fn print_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_print(expr))
  }

  fn return_statement(&mut self) -> StatementResult {
    let keyword = self.previous();
    let mut value = None;
    if !self.check(TokenType::Semicolon) {
      value = Some(self.expression()?);
    }

    self.consume(TokenType::Semicolon, "expected ';' after return value")?;
    return Ok(Stmt::new_return(keyword, value));
  }

  fn for_statement(&mut self) -> StatementResult {
    let token = self.previous();

    let initializer = if self.match_token(vec![TokenType::Var]) {
      self.var_decl()?
    } else {
      self.expr_statement()?
    };

    let condition = self.expression()?;
    self.consume(TokenType::Semicolon, "Expect ';' after condition")?;

    let increment = self.expression()?;
    self.consume(TokenType::LeftBrace, "Expect '{' after increment")?;

    let body = Stmt::new_block(vec![
      initializer,
      Stmt::new_while(
        token,
        condition,
        vec![
          Stmt::new_block(self.block()?),
          Stmt::new_expression(increment),
        ],
      ),
    ]);

    Ok(body)
  }

  fn while_statement(&mut self) -> StatementResult {
    let token = self.previous();
    let condition = self.expression()?;
    self.consume(TokenType::LeftBrace, "missing '{' after if condition")?;
    let body = self.block()?;
    Ok(Stmt::new_while(token, condition, body))
  }

  fn if_statement(&mut self) -> StatementResult {
    let condition = self.expression()?;
    self.consume(TokenType::LeftBrace, "missing '{' after if condition")?;
    let if_true = self.block()?;
    let mut if_false = None;

    if self.match_token(vec![TokenType::Else]) {
      if_false = if self.match_token(vec![TokenType::LeftBrace]) {
        Some(Box::new(Stmt::new_block(self.block()?)))
      } else if self.match_token(vec![TokenType::If]) {
        Some(Box::new(self.if_statement()?))
      } else {
        return Err(AstErr {
          file: String::from(self.file),
          line: self.peek().line,
          msg: format!("invalid token after token {}", self.peek()),
        });
      };
    }

    Ok(Stmt::new_if(condition, if_true, if_false))
  }

  fn expr_statement(&mut self) -> StatementResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_expression(expr))
  }

  fn load_statement(&mut self) -> StatementResult {
    let load = self.previous();
    let file = self.expression()?;
    self.consume(TokenType::Semicolon, "expected ';' after value")?;
    Ok(Stmt::new_load(load, file))
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
          file: String::from(self.file),
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
      file: String::from(self.file),
      line: self.peek().line,
      msg: String::from("could not find valid primary token"),
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

  fn peek(&self) -> &'b Token {
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
        file: String::from(self.file),
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

pub fn exec<'a>(file: &'a str, globals: EnvRef, prgm: Vec<Stmt>) -> ExprEvalResult {
  let mut e = Evaluator::new(String::from(file), globals);
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
  pub file: String,
  pub env: EnvRef,
}

impl Evaluator {
  fn new(file: String, env: EnvRef) -> Self {
    Self { file, env: env }
  }

  fn eval_expr(&mut self, e: &Expr) -> ExprEvalResult {
    expr::accept(e, self)
  }

  fn eval_stmt(&mut self, s: &Stmt) -> StmtEvalResult {
    stmt::accept(s, self)
  }

  pub fn eval_block(&mut self, statements: &Vec<Stmt>, env: EnvRef) -> StmtEvalResult {
    let prev_env = Rc::clone(&self.env);
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
  fn visit_expression_stmt(&mut self, e: &ExpressionStmt) -> StmtEvalResult {
    Ok(StatementType::Regular(self.eval_expr(&e.expr)?))
  }

  fn visit_print_stmt(&mut self, e: &PrintStmt) -> StmtEvalResult {
    println!("{}", self.eval_expr(&e.expr)?);
    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_var_stmt(&mut self, e: &VarStmt) -> StmtEvalResult {
    let mut value = Value::Nil;

    if let Some(i) = &e.initializer {
      value = self.eval_expr(i)?;
    }

    self.env.borrow_mut().define(e.name.lexeme.clone(), value);

    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_block_stmt(&mut self, e: &BlockStmt) -> StmtEvalResult {
    self.eval_block(
      &e.statements,
      Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(&self.env)))),
    )
  }

  fn visit_if_stmt(&mut self, e: &IfStmt) -> StmtEvalResult {
    let result = self.eval_expr(&e.condition)?;
    if self.is_truthy(&result) {
      self.eval_block(&e.if_true, Rc::clone(&self.env))
    } else if let Some(if_false) = &e.if_false {
      self.eval_stmt(&if_false)
    } else {
      Ok(StatementType::Regular(Value::Nil))
    }
  }

  fn visit_while_stmt(&mut self, e: &WhileStmt) -> StmtEvalResult {
    let mut result = StatementType::Regular(Value::Nil);

    loop {
      let res = self.eval_expr(&e.condition)?;
      if !self.is_truthy(&res) {
        break;
      }
      match self.eval_block(&e.body, Rc::clone(&self.env))? {
        StatementType::Regular(v) => result = StatementType::Regular(v),
        StatementType::Return(v) => {
          result = StatementType::Return(v);
          break;
        }
      }
    }

    Ok(result)
  }

  fn visit_function_stmt(&mut self, e: &FunctionStmt) -> StmtEvalResult {
    let name = e.name.lexeme.clone();
    let func = ScriptFunction::new(FunctionStmt::new(
      e.name.clone(),
      Rc::clone(&e.params),
      Rc::clone(&e.body),
    ));
    self
      .env
      .borrow_mut()
      .define(name, Value::Callee(Rc::new(func)));
    Ok(StatementType::Regular(Value::Nil))
  }

  fn visit_return_stmt(&mut self, s: &ReturnStmt) -> StmtEvalResult {
    let mut value = Value::Nil;
    if let Some(e) = &s.value {
      value = self.eval_expr(e)?;
    }

    Ok(StatementType::Return(value))
  }

  fn visit_load_stmt(&mut self, s: &LoadStmt) -> StmtEvalResult {
    let path = self.eval_expr(&s.path)?;
    if let Value::Str(path) = path {
      match fs::read_to_string(&path) {
        Ok(contents) => {
          let tokens = lex::analyze(&path, &contents)?;
          let program = parse(&path, &tokens.tokens)?;
          let result = exec(&self.file, Rc::clone(&self.env), program)?;
          Ok(StatementType::Regular(result))
        }
        Err(err) => Err(AstErr {
          file: self.file.clone(),
          line: s.load.line,
          msg: format!("failed loading file {}: {}", path, err),
        }),
      }
    } else {
      Err(AstErr {
        file: self.file.clone(),
        line: s.load.line,
        msg: format!("cannot load non string value"),
      })
    }
  }
}

impl ExprVisitor<ExprEvalResult> for Evaluator {
  fn visit_binary_expr(&mut self, e: &BinaryExpr) -> ExprEvalResult {
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

    Err(AstErr {
      file: self.file.clone(),
      line: e.operator.line,
      msg: format!(
        "Combination of {:?} and {:?} not allowed with {:?}",
        left, right, e.operator
      ),
    })
  }

  fn visit_ternary_expr(&mut self, e: &TernaryExpr) -> ExprEvalResult {
    let result = self.eval_expr(&e.condition)?;

    if self.is_truthy(&result) {
      self.eval_expr(&e.if_true)
    } else {
      self.eval_expr(&e.if_false)
    }
  }

  fn visit_grouping_expr(&mut self, e: &GroupingExpr) -> ExprEvalResult {
    self.eval_expr(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: &LiteralExpr) -> ExprEvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: &UnaryExpr) -> ExprEvalResult {
    let right = self.eval_expr(&e.right)?;

    match e.operator.token_type {
      TokenType::Exclamation => Ok(Value::Bool(!self.is_truthy(&right))),
      TokenType::Minus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(-n))
        } else {
          Err(AstErr {
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
            Err(err) => Err(AstErr {
              file: self.file.clone(),
              line: e.operator.line,
              msg: format!("string parse error: {}", err),
            }),
          }
        } else {
          Err(AstErr {
            file: self.file.clone(),
            line: e.operator.line,
            msg: format!("invalid absolution on type {}", right),
          })
        }
      }
      _ => Err(AstErr {
        file: self.file.clone(),
        line: e.operator.line,
        msg: format!("invalid unary operator {}", e.operator),
      }),
    }
  }

  fn visit_variable_expr(&mut self, e: &VariableExpr) -> ExprEvalResult {
    match self.env.borrow().lookup(&e.name.lexeme) {
      Some(v) => Ok(v.clone()),
      None => Err(AstErr {
        file: self.file.clone(),
        line: e.name.line,
        msg: format!("used uninitialized variable '{}'", e.name.lexeme),
      }),
    }
  }

  fn visit_assign_expr(&mut self, e: &AssignExpr) -> ExprEvalResult {
    let value = self.eval_expr(&e.value)?;
    if let Err(msg) = self
      .env
      .borrow_mut()
      .assign(e.name.lexeme.clone(), value.clone())
    {
      return Err(AstErr {
        file: self.file.clone(),
        line: e.name.line,
        msg: format!("assignment error: {}", msg),
      });
    }
    Ok(value)
  }

  fn visit_logical_expr(&mut self, e: &LogicalExpr) -> ExprEvalResult {
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
          file: self.file.clone(),
          line: e.operator.line,
          msg: String::from("invalid attempt for logical comparison"),
        })
      }
    }

    self.eval_expr(&e.right)
  }

  fn visit_range_expr(&mut self, e: &RangeExpr) -> ExprEvalResult {
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

    Err(AstErr {
      file: self.file.clone(),
      line: e.token.line,
      msg: String::from("expected number with range expression"),
    })
  }

  fn visit_call_expr(&mut self, e: &CallExpr) -> ExprEvalResult {
    let callee = self.eval_expr(&e.callee)?;

    if let Value::Callee(func) = callee {
      let mut args = Vec::new();
      for arg in e.args.iter() {
        args.push(self.eval_expr(arg)?);
      }
      Ok(func.call(self, args, e.paren.line)?)
    } else {
      Err(AstErr {
        file: self.file.clone(),
        line: e.paren.line,
        msg: format!("can't call type {}", callee),
      })
    }
  }

  fn visit_closure_expr(&mut self, e: &ClosureExpr) -> ExprEvalResult {
    Ok(Value::Callee(Rc::new(Closure::new(
      ClosureExpr::new(Rc::clone(&e.params), Rc::clone(&e.body)),
      Rc::clone(&self.env),
    ))))
  }
}
