use crate::ast::Evaluator;
use crate::expr::{
  self, AssignExpr, BinaryExpr, CallExpr, ClosureExpr, Expr, GetExpr, GroupingExpr, IsExpr,
  LiteralExpr, LogicalExpr, RangeExpr, SetExpr, TernaryExpr, UnaryExpr, VariableExpr,
};
use crate::lex::Token;
use crate::stmt::{
  self, BlockStmt, ClassStmt, ExpressionStmt, FunctionStmt, IfStmt, LetStmt, LoadStmt, LoadrStmt,
  PrintStmt, ReturnStmt, Stmt, WhileStmt,
};
use crate::types::Visitor;
use crate::ScriptError;
use std::collections::HashMap;

pub type ResolveResult = Result<(), ScriptError>;

pub fn resolve(evaluator: &mut Evaluator, statements: &Vec<Stmt>) -> ResolveResult {
  let mut resolver = Resolver::new(evaluator);
  resolver.resolve_statements(statements)
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
      if scope.insert(name.to_string(), false).is_some() {
        return Err(ScriptError {
          file: self.evaluator.file.clone(),
          line: name.line,
          msg: format!("variable in scope already declared with name '{}'", name),
        });
      }
    }
    Ok(())
  }

  fn define(&mut self, name: &Token) -> Result<(), ScriptError> {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(name.to_string(), true);
      Ok(())
    } else {
      Err(ScriptError {
        file: self.evaluator.file.clone(),
        line: name.line,
        msg: format!("unable to define undeclared variable '{}'", name),
      })
    }
  }

  fn resolve_statements(&mut self, statements: &Vec<Stmt>) -> ResolveResult {
    for stmt in statements.iter() {
      self.resolve_statement(stmt)?;
    }
    Ok(())
  }

  fn resolve_statement(&mut self, s: &Stmt) -> ResolveResult {
    stmt::accept(s, self)
  }

  fn resolve_function(&mut self, s: &FunctionStmt) -> ResolveResult {
    self.begin_function();
    for param in s.params.iter() {
      self.declare(param)?;
      self.define(param)?;
    }
    self.resolve_statements(&*s.body)?;
    self.end_function();
    Ok(())
  }

  fn resolve_closure(&mut self, e: &ClosureExpr) -> ResolveResult {
    self.begin_function();
    for param in e.params.iter() {
      self.declare(param)?;
      self.define(param)?;
    }
    self.resolve_statements(&e.body)?;
    self.end_function();
    Ok(())
  }

  fn resolve_expression(&mut self, e: &Expr) -> ResolveResult {
    expr::accept(e, self)
  }

  fn resolve_local_variable(&mut self, e: &VariableExpr) {
    self.resolve_local(&e.name.to_string(), e.id);
  }

  fn resolve_local_assignment(&mut self, e: &AssignExpr) {
    self.resolve_local(&e.name.to_string(), e.id);
  }

  fn resolve_local(&mut self, name: &str, id: usize) {
    for i in (0..self.scopes.len()).rev() {
      let scope = &self.scopes[i];
      if scope.get(name).is_some() {
        let depth = self.scopes.len() - 1 - i;
        self.evaluator.resolve(id, depth);
        break;
      }
    }
  }
}

impl Visitor<VariableExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &VariableExpr) -> ResolveResult {
    if let Some(scope) = self.scopes.last() {
      if let Some(v) = scope.get(&e.name.to_string()) {
        if !v {
          return Err(ScriptError {
            file: self.evaluator.file.clone(),
            line: e.name.line,
            msg: String::from("can't read local variable in its own initializer"),
          });
        }
      }
    }

    self.resolve_local_variable(e);
    Ok(())
  }
}

impl Visitor<AssignExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &AssignExpr) -> ResolveResult {
    self.resolve_expression(&*e.value)?;
    self.resolve_local_assignment(e);
    Ok(())
  }
}

impl Visitor<BinaryExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &BinaryExpr) -> ResolveResult {
    self.resolve_expression(&e.left)?;
    self.resolve_expression(&e.right)
  }
}

impl Visitor<CallExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &CallExpr) -> ResolveResult {
    self.resolve_expression(&*e.callee)?;
    for arg in e.args.iter() {
      self.resolve_expression(arg)?;
    }
    Ok(())
  }
}

impl Visitor<GetExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &GetExpr) -> ResolveResult {
    self.resolve_expression(&e.object)
  }
}

impl Visitor<GroupingExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &GroupingExpr) -> ResolveResult {
    self.resolve_expression(&e.expression)
  }
}

impl Visitor<LiteralExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, _: &LiteralExpr) -> ResolveResult {
    Ok(())
  }
}

impl Visitor<LogicalExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &LogicalExpr) -> ResolveResult {
    self.resolve_expression(&e.left)?;
    self.resolve_expression(&e.right)
  }
}

impl Visitor<SetExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &SetExpr) -> ResolveResult {
    self.resolve_expression(&e.value)?;
    self.resolve_expression(&e.object)
  }
}

impl Visitor<UnaryExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &UnaryExpr) -> ResolveResult {
    self.resolve_expression(&e.right)
  }
}

impl Visitor<ClosureExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &ClosureExpr) -> ResolveResult {
    self.resolve_closure(e)
  }
}

impl Visitor<RangeExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &RangeExpr) -> ResolveResult {
    self.resolve_expression(&e.begin)?;
    self.resolve_expression(&e.end)
  }
}

impl Visitor<IsExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &IsExpr) -> ResolveResult {
    self.resolve_expression(&e.value)
  }
}

impl Visitor<TernaryExpr, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, e: &TernaryExpr) -> ResolveResult {
    self.resolve_expression(&e.condition)?;
    self.resolve_expression(&e.if_true)?;
    self.resolve_expression(&e.if_false)
  }
}

impl Visitor<BlockStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &BlockStmt) -> ResolveResult {
    self.begin_scope();
    self.resolve_statements(&s.statements)?;
    self.end_scope();
    Ok(())
  }
}

impl Visitor<ClassStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &ClassStmt) -> ResolveResult {
    self.declare(&s.name)?;
    self.define(&s.name)?;
    self.begin_scope();
    self.resolve_statements(&s.methods)?;
    self.end_scope();
    Ok(())
  }
}

impl Visitor<LetStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &LetStmt) -> ResolveResult {
    self.declare(&s.name)?;
    if let Some(i) = &s.initializer {
      self.resolve_expression(i)?;
    }
    self.define(&s.name)?;
    Ok(())
  }
}

impl Visitor<FunctionStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &FunctionStmt) -> ResolveResult {
    self.declare(&s.name)?;
    self.define(&s.name)?;
    self.resolve_function(s)
  }
}

impl Visitor<ExpressionStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &ExpressionStmt) -> ResolveResult {
    self.resolve_expression(&s.expr)
  }
}

impl Visitor<IfStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &IfStmt) -> ResolveResult {
    self.resolve_expression(&s.condition)?;
    self.begin_scope();
    self.resolve_statements(&s.if_true)?;
    self.end_scope();
    if let Some(if_false) = &s.if_false {
      self.begin_scope();
      self.resolve_statement(if_false)?;
      self.end_scope();
    }
    Ok(())
  }
}

impl Visitor<PrintStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &PrintStmt) -> ResolveResult {
    self.resolve_expression(&s.expr)
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
      self.resolve_expression(v)?;
    }
    Ok(())
  }
}

impl Visitor<WhileStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &WhileStmt) -> ResolveResult {
    self.resolve_expression(&s.condition)?;
    self.begin_scope();
    self.resolve_statements(&s.body)?;
    self.end_scope();
    Ok(())
  }
}

impl Visitor<LoadStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &LoadStmt) -> ResolveResult {
    self.resolve_expression(&s.path)
  }
}

impl Visitor<LoadrStmt, ResolveResult> for Resolver<'_> {
  fn visit(&mut self, s: &LoadrStmt) -> ResolveResult {
    self.resolve_expression(&s.path)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::env::EnvRef;
  use crate::lex::TokenKind;
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

      let curr = r.scopes.len();
      r.begin_scope();
      assert_eq!(r.scopes.len(), curr + 1);
    }

    #[test]
    fn resolver_should_pop_a_scope_on_end_scope() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let curr = r.scopes.len();
      r.begin_scope();
      r.end_scope();
      assert_eq!(r.scopes.len(), curr);
    }

    #[test]
    fn resolver_should_increment_function_depth_and_push_scope_on_begin_function() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let curr = r.scopes.len();
      r.begin_function();
      assert_eq!(r.function_depth, 1);
      assert_eq!(r.scopes.len(), curr + 1);
    }

    #[test]
    fn resolver_should_decrement_function_depth_and_pop_scope_on_end_function() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let curr = r.scopes.len();
      r.begin_function();
      r.end_function();
      assert_eq!(r.function_depth, 0);
      assert_eq!(r.scopes.len(), curr);
    }

    #[test]
    fn resolver_should_declare_a_variable_only_if_it_was_not_previously_declared() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let token = Token::new(TokenKind::Identifier(String::from("foo")), 1);

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

      let token = Token::new(TokenKind::Identifier(String::from("foo")), 1);

      r.begin_scope();
      assert!(r.declare(&token).is_ok());
      assert!(r.declare(&token).is_err());
    }

    #[test]
    fn resolving_a_function_stmt() {
      let env = EnvRef::default();
      let mut e = Evaluator::new("test".into(), env);
      let mut r = Resolver::new(&mut e);

      let func_name = Token::new(TokenKind::Identifier(String::from("foo")), 1);
      let param_name = Token::new(TokenKind::Identifier(String::from("bar")), 2);
      let local_name = Token::new(TokenKind::Identifier(String::from("foobar")), 3);
      let params = vec![param_name];
      let body = vec![Stmt::Let(LetStmt::new(local_name, None, 4))];
      let s = FunctionStmt::new(func_name, Rc::new(params), Rc::new(body), 5);

      assert!(r.resolve_function(&s).is_ok());
    }
  }

  #[cfg(test)]
  mod integration {
    use super::*;
    #[test]
    fn resolver_should_not_allow_variable_in_parent_scope_to_be_replaced_in_function_after_redeclaration_in_child_scope(
    ) {
      const SRC: &str = r#"
      let var = "global";
      {
        fn check_fn() {
          assert(var, "global");
        }

        let check_closure = || {
          assert(var, "global");
        };

        check_fn();
        check_closure();

        let var = "local";

        check_fn();
        check_closure();

        fn check_fn_again() {
          assert(var, "local");
        }

        let check_closure_again = || {
          assert(var, "local");
        };

        check_fn_again();
        check_closure_again();
      }

      let num = 100;
      fn base() {
        assert(num, 100);
      }

      fn test() {
        let num = 200;
        base();
      }

      test();
      "#;
      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_allow_variables_to_leak_into_class_methods() {
      const SRC: &str = r#"
      let var = "global";

      class Test {
        fn check_fn() {
          assert(var, "global");
        }
      }

      let t = Test();
      t.closure = || {
        assert(var, "global");
      };
      t.closure();
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_not_mixup_local_variables_and_class_instance_variables() {
      const SRC: &str = r#"
      let x = "local";
      class Test {
      }

      let t = Test();
      t.x = "member";

      assert(x, "local");
      assert(t.x, "member");
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_not_mixup_local_functions_and_class_member_functions() {
      const SRC: &str = r#"
      let x = "local";
      class Test {
        fn test(self) {
          return 1;
        }
      }

      fn test() {
        return 2;
      }

      let t = Test();

      assert(test(), 2);
      assert(t.test(), 1);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_still_work_with_closures_declared_in_member_functions() {
      const SRC: &str = r#"
      let x = "global";
      class Test {
        fn test(self) {
          let x = "local member";
          return || {
            return x;
          };
        }
      }

      let t = Test();
      let func = t.test();
      assert(func(), "local member");
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

    #[test]
    fn resolver_should_not_allow_redeclaration_of_fn_parameters() {
      const SRC: &str = r#"
      fn function(param) {
        let param = "invalid";
      }
      "#;
      let i = Interpreter::new_with_test_support();
      assert!(i.exec(&"test".into(), SRC).is_err());
    }

    #[test]
    fn resolver_should_find_variables_within_if_statements() {
      const SRC: &str = r#"
      let e = 1;

      if true {
        e = 2;
      }

      assert(e, 2);
      "#;
      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_find_variables_within_while_statements() {
      const SRC: &str = r#"
      let e = true;

      while e {
        e = false;
      }
      "#;
      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_find_variables_within_for_statements() {
      const SRC: &str = r#"
      let e;

      for let x = 0; x < 1; x = x + 1 {
        e = x;
        if true {
          e = x + 1;
        }
      }

      assert(e, 1);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_bind_to_lambda_parameters() {
      const SRC: &str = r#"
      fn new_test() {
        |x, y, z| {
          if x < 1 or y < 1 {
            return;
          }
          assert(x, 1);
          assert(y, 2);
          z(x - 1, y - 1, z);
        };
      }

      let test = new_test();
      test(1, 2, test);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_should_bind_to_fn_params() {
      const SRC: &str = r#"
      fn outer(a) {
        fn inner(b) {
          return a + b;
        }

        return inner;
      }

      let f1 = outer(1);
      assert(f1(1), 2);

      let f2 = outer(2);
      assert(f1(1), 2);
      assert(f2(1), 3);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }

    #[test]
    fn resolver_allows_for_recursion() {
      const SRC: &str = r#"
      fn rec(n) {
        if n == 0 {
          return;
        }

        rec(n - 1);
      }

      rec(10);
      "#;

      let i = Interpreter::new_with_test_support();
      if let Err(err) = i.exec(&"test".into(), SRC) {
        panic!(format!("{}", err));
      }
    }
  }
}
