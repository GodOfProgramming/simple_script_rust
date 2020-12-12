use crate::ast::Evaluator;
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GetExpr, GroupingExpr, LiteralExpr,
  LogicalExpr, RangeExpr, SetExpr, TernaryExpr, UnaryExpr, VariableExpr,
};
use crate::lex::Token;
use crate::stmt::{
  self, BlockStmt, ClassStmt, ExpressionStmt, FunctionStmt, IfStmt, LoadStmt, LoadrStmt, PrintStmt,
  ReturnStmt, Stmt, LetStmt, WhileStmt,
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
      scopes: Vec::new(),
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

impl Visitor<GetExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &GetExpr) -> ResolveResult {
    self.resolve(&*e.object)
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

impl Visitor<SetExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &SetExpr) -> ResolveResult {
    self.resolve(&*e.value)?;
    self.resolve(&*e.object)
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

impl Visitor<ClassStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &ClassStmt) -> ResolveResult {
    self.declare(&s.name)?;
    self.define(&s.name);
    self.begin_scope();
    self.resolve(&s.methods)?;
    self.end_scope();
    Ok(())
  }
}

impl Visitor<LetStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &LetStmt) -> ResolveResult {
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
      });
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
  use super::*;
  use crate::env::EnvRef;
  use crate::lex::TokenType;
  use crate::Interpreter;
  use std::rc::Rc;

  #[cfg(test)]
  mod unit {
    use super::*;
    #[test]
    fn resolver_should_push_a_scope_on_begin_scope() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      r.begin_scope();
      assert_eq!(r.scopes.len(), 1);
    }

    #[test]
    fn resolver_should_pop_a_scope_on_end_scope() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      r.begin_scope();
      r.end_scope();
      assert_eq!(r.scopes.len(), 0);
    }

    #[test]
    fn resolver_should_increment_function_depth_and_push_scope_on_begin_function() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);
      r.begin_function();
      assert_eq!(r.function_depth, 1);
      assert_eq!(r.scopes.len(), 1);
    }

    #[test]
    fn resolver_should_decrement_function_depth_and_pop_scope_on_end_function() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);
      r.begin_function();
      r.end_function();
      assert_eq!(r.function_depth, 0);
      assert_eq!(r.scopes.len(), 0);
    }

    #[test]
    fn resolver_should_declare_a_variable_only_if_it_was_not_previously_declared() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let token = Token::new(TokenType::Identifier, String::from("foo"), None, 1);

      r.begin_scope();
      assert!(r.declare(&token).is_ok());

      if let Some(scope) = r.scopes.last() {
        let value = scope.get(&String::from("foo"));
        assert!(value.is_some());
        if let Some(is_defined) = value {
          assert!(!is_defined);
        }
      } else {
        panic!("scope not pushed in test");
      }
    }

    #[test]
    fn resolver_should_not_declare_a_variable_if_it_was_previously_declared() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let token = Token::new(TokenType::Identifier, String::from("foo"), None, 1);

      r.begin_scope();
      assert!(r.declare(&token).is_ok());
      assert!(r.declare(&token).is_err());
    }

    #[test]
    fn resolving_a_function_stmt() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let func_name = Token::new(TokenType::Identifier, String::from("foo"), None, 1);
      let param_name = Token::new(TokenType::Identifier, String::from("bar"), None, 2);
      let local_name = Token::new(TokenType::Identifier, String::from("foobar"), None, 3);
      let params = vec![param_name];
      let body = vec![Stmt::Let(LetStmt::new(local_name, None, 4))];
      let s = FunctionStmt::new(func_name, Rc::new(params), Rc::new(body), 5);

      assert!(r.resolve(&s).is_ok());
    }

    #[test]
    fn t() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);
    }
  }

  #[cfg(test)]
  mod integration {
    use super::*;
    #[test]
    fn resolver_should_not_allow_variable_in_parent_scope_to_be_replaced_in_function_after_redeclaration_in_child_scope(
    ) {
      const SRC: &str = r#"
    let a = "global";

    class Test {
      fn check_a_fn() {
        assert(a, "global");
      }
    }

    let g = Test();
    {
      fn check_a_fn() {
        assert(a, "global");
      }

      let check_a_closure = || {
        assert(a, "global");
      };

      let l1 = Test();

      check_a_fn();
      check_a_closure();

      g.check_a_fn();
      l1.check_a_fn();

      let a = "local";

      check_a_fn();
      check_a_closure();

      g.check_a_fn();
      l1.check_a_fn();

      let l2 = Test();

      l2.check_a_fn();

      class Test2 {
        fn check_a_fn() {
          assert(a, "local");
        }
      }

      fn check_a_fn_again() {
        assert(a, "local");
      }

      let check_a_closure_again = || {
        assert(a, "local");
      };

      check_a_fn_again();
      check_a_closure_again();

      let t3 = Test2();

      t3.check_a_fn();
    }
    "#;
      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
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
}
