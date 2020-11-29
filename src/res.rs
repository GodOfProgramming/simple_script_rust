use crate::ast::Evaluator;
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GroupingExpr, LiteralExpr,
  LogicalExpr, RangeExpr, TernaryExpr, UnaryExpr, VariableExpr,
};
use crate::lex::Token;
use crate::stmt::{
  self, BlockStmt, ExpressionStmt, FunctionStmt, IfStmt, LoadStmt, LoadrStmt, PrintStmt,
  ReturnStmt, Stmt, VarStmt, WhileStmt,
};
use crate::types::Visitor;
use crate::ScriptError;
use std::collections::HashMap;

pub type ResolveResult = Result<(), ScriptError>;

pub fn resolve(evaluator: &mut Evaluator) {}

struct Resolver<'eval> {
  evaluator: &'eval Evaluator,
  scopes: Vec<HashMap<String, bool>>,
}

impl<'eval> Resolver<'eval> {
  fn new(evaluator: &'eval Evaluator) -> Self {
    Self {
      evaluator,
      scopes: Vec::new(),
    }
  }

  fn begin_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn end_scope(&mut self) {
    self.scopes.pop();
  }

  fn declare(&mut self, name: &Token) {
    if let Some(scope) = self.scopes.last() {
      scope.insert(name.lexeme.clone(), false);
    }
  }

  fn define(&mut self, name: &Token) {
    if let Some(scope) = self.scopes.last() {
      scope.insert(name.lexeme.clone(), true);
    }
  }
}

trait Res<T> {
  type Return;
  fn resolve(&mut self, input: &T) -> Self::Return;
}

impl Res<Vec<Stmt>> for Resolver<'_> {
  type Return = ResolveResult;
  fn resolve(&mut self, statements: &Vec<Stmt>) -> Self::Return {
    for stmt in statements.iter() {
      self.resolve(stmt)?;
    }
    Ok(())
  }
}

impl Res<Stmt> for Resolver<'_> {
  type Return = ResolveResult;
  fn resolve(&mut self, s: &Stmt) -> Self::Return {
    stmt::accept(s, self)
  }
}

impl Res<FunctionStmt> for Resolver<'_> {
  type Return = ResolveResult;
  fn resolve(&mut self, s: &FunctionStmt) -> Self::Return {
    self.begin_scope();
    for param in s.params.iter() {
      self.declare(param);
      self.define(param);
    }
    self.resolve(&*s.body)?;
    self.end_scope();
    Ok(())
  }
}

impl Res<Expr> for Resolver<'_> {
  type Return = ResolveResult;
  fn resolve(&mut self, e: &Expr) -> Self::Return {
    expr::accept(e, self)
  }
}

trait ResLoc<T> {
  fn resolve_local(&mut self, t: &T, name: &Token);
}

impl ResLoc<VariableExpr> for Resolver<'_> {
  fn resolve_local(&mut self, e: &VariableExpr, name: &Token) {
    for (i, scope) in self.scopes.iter().rev().enumerate() {
      if let Some(_) = scope.get(&name.lexeme) {
        self.evaluator.resolve(e, self.scopes.len() - 1 - i);
      }
    }
  }
}

impl ResLoc<AssignExpr> for Resolver<'_> {
  fn resolve_local(&mut self, e: &AssignExpr, name: &Token) {
    for (i, scope) in self.scopes.iter().rev().enumerate() {
      if let Some(_) = scope.get(&name.lexeme) {
        self.evaluator.resolve(e, self.scopes.len() - 1 - i);
      }
    }
  }
}

impl Visitor<VariableExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &VariableExpr) -> ResolveResult {
    if let Some(scope) = self.scopes.last() {
      if !scope[&e.name.lexeme] {
        return Err(ScriptError {
          file: "TODO".into(),
          line: e.name.line,
          msg: String::from("can't read local variable in its own initializer"),
        });
      }
    }

    self.resolve_local(e, &e.name);
    Ok(())
  }
}

impl Visitor<AssignExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &AssignExpr) -> ResolveResult {
    self.resolve(&*e.value)?;
    self.resolve_local(e, &e.name);
    Ok(())
  }
}

impl Visitor<BinaryExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &BinaryExpr) -> ResolveResult {
    self.resolve(&*e.left)?;
    self.resolve(&*e.right)
  }
}

impl Visitor<CallExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &CallExpr) -> ResolveResult {
    self.resolve(&*e.callee)?;
    for arg in e.args.iter() {
      self.resolve(arg)?;
    }
    Ok(())
  }
}

impl Visitor<GroupingExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &GroupingExpr) -> ResolveResult {
    self.resolve(&*e.expression)
  }
}

impl Visitor<LiteralExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &LiteralExpr) -> ResolveResult {
    Ok(())
  }
}

impl Visitor<LogicalExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &LogicalExpr) -> ResolveResult {
    self.resolve(&*e.left)?;
    self.resolve(&*e.right)
  }
}

impl Visitor<UnaryExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &UnaryExpr) -> ResolveResult {
    self.resolve(&*e.right)
  }
}

impl Visitor<BlockStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &BlockStmt) -> ResolveResult {
    self.begin_scope();
    self.resolve(&s.statements)?;
    self.end_scope();
    Ok(())
  }
}

impl Visitor<VarStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &VarStmt) -> ResolveResult {
    self.declare(&s.name);
    if let Some(i) = &s.initializer {
      self.resolve(i)?;
    }
    self.define(&s.name);
    Ok(())
  }
}

impl Visitor<FunctionStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &FunctionStmt) -> ResolveResult {
    self.declare(&s.name);
    self.define(&s.name);
    self.resolve(s)
  }
}

impl Visitor<ExpressionStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &ExpressionStmt) -> ResolveResult {
    self.resolve(&s.expr)
  }
}

impl Visitor<IfStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &IfStmt) -> ResolveResult {
    self.resolve(&s.condition)?;
    self.resolve(&s.if_true)?;
    if let Some(if_false) = &s.if_false {
      self.resolve(&**if_false)?;
    }
    Ok(())
  }
}

impl Visitor<PrintStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &PrintStmt) -> ResolveResult {
    self.resolve(&s.expr)
  }
}

impl Visitor<ReturnStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &ReturnStmt) -> ResolveResult {
    if let Some(v) = &s.value {
      self.resolve(v)?;
    }
    Ok(())
  }
}

impl Visitor<WhileStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &WhileStmt) -> ResolveResult {
    self.resolve(&s.condition)?;
    self.resolve(&s.body)
  }
}