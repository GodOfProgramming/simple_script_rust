use crate::ast::{self, AstErr};
use crate::builtin;
use crate::env::{Env, EnvRef};
use crate::lex::{self, LexicalErr};
use crate::types::Value;
use std::cell::RefCell;
use std::fmt::{self, Display};
use std::rc::Rc;
use std::sync::Mutex;

pub struct ExecResult {
  pub value: Value,
  pub lines: usize,
}

#[derive(Debug)]
pub struct ExecErr {
  pub msg: String,
  pub line: usize,
}

impl Display for ExecErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.line, self.msg)
  }
}

impl From<AstErr> for ExecErr {
  fn from(err: AstErr) -> Self {
    Self {
      msg: err.msg,
      line: err.line,
    }
  }
}

impl From<LexicalErr> for ExecErr {
  fn from(err: LexicalErr) -> Self {
    Self {
      msg: err.msg,
      line: err.line,
    }
  }
}

pub struct Interpreter {
  globals: Mutex<EnvRef>,
}

impl Interpreter {
  pub fn new() -> Interpreter {
    let mut globals = Env::new();

    builtin::time::enable(&mut globals);

    Interpreter {
      globals: Mutex::new(Rc::new(RefCell::new(globals))),
    }
  }

  pub fn exec(&self, src: &str) -> Result<ExecResult, ExecErr> {
    let res = lex::analyze(src)?;
    let program = ast::parse(&res.tokens)?;
    let value = ast::exec(Rc::clone(&self.globals.lock().unwrap()), program)?;

    Ok(ExecResult {
      value,
      lines: res.lines,
    })
  }

  pub fn set_var(&mut self, name: &String, value: Value) {
    self
      .globals
      .lock()
      .unwrap()
      .borrow_mut()
      .define(name.clone(), value);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  const INTEGER_SCRIPT: &str = "12345;";
  const STRING_SCRIPT: &str = r#""some string";"#;
  const TRUE_SCRIPT: &str = "true;";
  const FALSE_SCRIPT: &str = "false;";
  const NIL_SCRIPT: &str = "nil;";

  fn handle_result(expected: Value, res: Result<ExecResult, ExecErr>) {
    match res {
      Ok(e) => {
        assert_eq!(e.lines, 0);
        assert_eq!(expected, e.value)
      }
      Err(err) => println!("{}", err),
    }
  }

  #[test]
  fn test_exec() {
    let i = Interpreter::new();

    let results = vec![
      Value::Num(12345.0),
      Value::Str(String::from("some string")),
      Value::Bool(true),
      Value::Bool(false),
      Value::Nil,
    ];
    let tests = vec![
      INTEGER_SCRIPT,
      STRING_SCRIPT,
      TRUE_SCRIPT,
      FALSE_SCRIPT,
      NIL_SCRIPT,
    ];

    for (res, test) in results.into_iter().zip(tests.into_iter()) {
      handle_result(res, i.exec(test))
    }
  }

  const CLOSURE_INIT_SCRIPT: &str = "|a, b| {
    a + b;
  };";

  const CLOSURE_CALL_SCRIPT: &str = "closure(1, 2);";

  #[test]
  fn transfer_closure() {
    let i1 = Interpreter::new();
    let mut i2 = Interpreter::new();

    let closure = i1.exec(CLOSURE_INIT_SCRIPT).unwrap();
    i2.set_var(&String::from("closure"), closure.value);

    handle_result(Value::Num(3.0), i2.exec(CLOSURE_CALL_SCRIPT));
  }
}
