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

pub fn resolve(evaluator: &mut Evaluator, statements: &Vec<Stmt>) -> ResolveResult {
  let mut resolver = Resolver::new(evaluator);
  resolver.resolve(statements)
}

struct Resolver<'eval> {
  evaluator: &'eval mut Evaluator,
  scopes: Vec<HashMap<String, bool>>,
  function_depth: usize,
}

impl<'eval> Resolver<'eval> {
  fn new(evaluator: &'eval mut Evaluator) -> Self {
    Self {
      evaluator,
      scopes: vec![HashMap::new()],
      function_depth: 0,
    }
  }

  fn begin_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn end_scope(&mut self) {
    self.scopes.pop();
  }

  fn begin_function(&mut self) {
    self.function_depth += 1;
    self.begin_scope();
  }

  fn end_function(&mut self) {
    self.function_depth -= 1;
    self.end_scope();
  }

  fn declare(&mut self, name: &Token) -> Result<(), ScriptError> {
    if let Some(scope) = self.scopes.last_mut() {
      if scope.insert(name.lexeme.clone(), false).is_some() {
        return Err(ScriptError {
          file: self.evaluator.file.clone(),
          line: name.line,
          msg: format!(
            "variable in scope already declared with name '{}'",
            name.lexeme
          ),
        });
      }
    }
    Ok(())
  }

  fn define(&mut self, name: &Token) {
    if let Some(scope) = self.scopes.last_mut() {
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
    self.begin_function();
    for param in s.params.iter() {
      self.declare(param)?;
      self.define(param);
    }
    self.resolve(&*s.body)?;
    self.end_function();
    Ok(())
  }
}

impl Res<ClosureExpr> for Resolver<'_> {
  type Return = ResolveResult;
  fn resolve(&mut self, e: &ClosureExpr) -> Self::Return {
    self.begin_function();
    for param in e.params.iter() {
      self.declare(param)?;
      self.define(param);
    }
    self.resolve(&*e.body)?;
    self.end_function();
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
    // will be empty when using variables defined external to scripts
    if !self.scopes.is_empty() {
      for i in (0..self.scopes.len() - 1).rev() {
        let scope = &self.scopes[i];
        if scope.get(&name.lexeme).is_some() {
          let depth = self.scopes.len() - 1 - i;
          self.evaluator.resolve(e.id, depth);
          break;
        }
      }
    }
  }
}

impl ResLoc<AssignExpr> for Resolver<'_> {
  fn resolve_local(&mut self, e: &AssignExpr, name: &Token) {
    for i in (0..self.scopes.len() - 1).rev() {
      let scope = &self.scopes[i];
      if scope.get(&name.lexeme).is_some() {
        let depth = self.scopes.len() - 1 - self.scopes.len() - i;
        self.evaluator.resolve(e.id, depth);
        break;
      }
    }
  }
}

impl Visitor<VariableExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &VariableExpr) -> ResolveResult {
    if let Some(scope) = self.scopes.last() {
      if let Some(v) = scope.get(&e.name.lexeme) {
        if !v {
          return Err(ScriptError {
            file: self.evaluator.file.clone(),
            line: e.name.line,
            msg: String::from("can't read local variable in its own initializer"),
          });
        }
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
  fn visit(&mut self, _: &LiteralExpr) -> ResolveResult {
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

impl Visitor<ClosureExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &ClosureExpr) -> ResolveResult {
    self.resolve(e)
  }
}

impl Visitor<RangeExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &RangeExpr) -> ResolveResult {
    self.resolve(&*e.begin)?;
    self.resolve(&*e.end)
  }
}

impl Visitor<TernaryExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &TernaryExpr) -> ResolveResult {
    self.resolve(&*e.condition)?;
    self.resolve(&*e.if_true)?;
    self.resolve(&*e.if_false)
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
    self.declare(&s.name)?;
    if let Some(i) = &s.initializer {
      self.resolve(i)?;
    }
    self.define(&s.name);
    Ok(())
  }
}

impl Visitor<FunctionStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &FunctionStmt) -> ResolveResult {
    self.declare(&s.name)?;
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
    if self.function_depth == 0 {
      return Err(ScriptError {
        file: self.evaluator.file.clone(),
        line: s.keyword.line,
        msg: String::from("can't return outside of a function"),
      })
    }
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

impl Visitor<LoadStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &LoadStmt) -> ResolveResult {
    self.resolve(&s.path)
  }
}

impl Visitor<LoadrStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &LoadrStmt) -> ResolveResult {
    self.resolve(&s.path)
  }
}

#[cfg(test)]
mod tests {
  use crate::Interpreter;

  #[test]
  fn resolver_should_not_allow_variable_in_parent_scope_to_be_replaced_in_function_after_redeclaration_in_child_scope(
  ) {
    const SRC: &str = r#"
    let a = "global";
    {
      fn check_a_fn() {
        assert(a, "global");
      }

      let check_a_closure = || {
        assert(a, "global");
      };

      check_a_fn();
      check_a_closure();

      let a = "local";

      check_a_fn();
      check_a_closure();
    }
    "#;
    let i = Interpreter::new_with_test_support();
    assert!(i.exec(&"test".into(), SRC).is_ok());
  }

  #[test]
  fn resolver_should_not_allow_two_variables_with_the_same_name_globally() {
    const SRC: &str = r#"
    let a = "true";
    let a = "false";
    "#;
    let i = Interpreter::new_with_test_support();
    let res = i.exec(&"test".into(), SRC);

    match res {
      Ok(_) => panic!("failed to detect duplicate variable"),
      Err(e) => {
        assert_eq!(e.msg, "variable in scope already declared with name 'a'");
      }
    };
  }

  #[test]
  fn resolver_should_not_allow_two_variables_with_the_same_name_in_functions() {
    const SRC: &str = r#"
    fn foo() {
      let a = "true";
      let a = "false";
    }
    "#;
    let i = Interpreter::new_with_test_support();
    let res = i.exec(&"test".into(), SRC);

    match res {
      Ok(_) => panic!("failed to detect duplicate variable"),
      Err(e) => {
        assert_eq!(e.msg, "variable in scope already declared with name 'a'");
      }
    };
  }

  #[test]
  fn resolver_should_not_allow_a_return_statement_outside_of_a_function() {
    const SRC: &str = r#"
    return true;
    "#;
    let i = Interpreter::new_with_test_support();
    let res = i.exec(&"test".into(), SRC);

    match res {
      Ok(_) => panic!("failed to detect external return statement"),
      Err(e) => {
        assert_eq!(e.msg, "can't return outside of a function");
      }
    };
  }

  #[test]
  fn resolver_should_allow_a_return_statement_inside_of_a_function() {
    const SRC: &str = r#"
    fn some_func() {
      return true;
    }
    "#;
    let i = Interpreter::new_with_test_support();
    assert!(i.exec(&"test".into(), SRC).is_ok());
  }
}
