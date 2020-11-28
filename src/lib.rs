use crate::ast::AstErr;
use crate::env::{Env, EnvRef};
use crate::lex::LexicalErr;
use crate::types::Value;
use std::fmt::{self, Display};
use std::io::{self, Write};
use std::rc::Rc;

pub mod types;

mod ast;
mod builtin;
mod env;
mod expr;
mod lex;
mod stmt;

pub type ExecResult = Result<Value, ExecErr>;

#[derive(Debug)]
pub struct ExecErr {
  pub file: String,
  pub line: usize,
  pub msg: String,
}

impl Display for ExecErr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} ({}): {}", self.file, self.line, self.msg)
  }
}

impl From<AstErr> for ExecErr {
  fn from(err: AstErr) -> Self {
    Self {
      file: err.file,
      line: err.line,
      msg: err.msg,
    }
  }
}

impl From<LexicalErr> for ExecErr {
  fn from(err: LexicalErr) -> Self {
    Self {
      file: err.file,
      line: err.line,
      msg: err.msg,
    }
  }
}

pub struct Interpreter {
  globals: EnvRef,
}

impl Default for Interpreter {
  fn default() -> Self {
    let globals = Env::new_ref();

    builtin::time::enable(Rc::clone(&globals));
    builtin::meta::enable(Rc::clone(&globals));

    Interpreter { globals }
  }
}

impl Interpreter {
  pub fn default_with_test_support() -> Self {
    let i = Interpreter::default();

    builtin::test::enable(Rc::clone(&i.globals));

    i
  }

  pub fn set_var(&mut self, name: &str, value: Value) {
    self.globals.borrow_mut().define(name.to_string(), value);
  }

  pub fn exec(&self, script_name: &str, src: &str) -> Result<Value, ExecErr> {
    let res = lex::analyze("ss", src)?;
    let program = ast::parse(script_name, &res.tokens)?;
    let value = ast::exec(script_name, Rc::clone(&self.globals), program)?;
    Ok(value)
  }

  pub fn cli(&mut self) -> bool {
    let mut input = String::new();
    let mut exit = false;
    let mut line_number = 1;

    while !exit {
      input.clear();
      print!("ss(main):{}> ", line_number);
      if let Err(err) = io::stdout().flush() {
        println!("{}", err);
        return false;
      }

      if let Err(err) = io::stdin().read_line(&mut input) {
        println!("{}", err);
        return false;
      }

      if input == "exit" {
        exit = true;
      }

      let analysis = match lex::analyze("ss", &input) {
        Ok(a) => a,
        Err(err) => {
          // - 1 because analyze will read the \n from pressing enter
          println!("{} ({}): {}", err.file, err.line + line_number - 1, err.msg);
          continue;
        }
      };

      let program = match ast::parse("ss", &analysis.tokens) {
        Ok(p) => p,
        Err(err) => {
          println!("{} ({}): {}", err.file, err.line + line_number, err.msg);
          continue;
        }
      };

      match ast::exec("ss", Rc::clone(&self.globals), program) {
        Ok(v) => {
          println!("=> {}", v);
          line_number += analysis.lines_analyzed;
        }
        Err(err) => println!("{} ({}): {}", err.file, err.line + line_number, err.msg),
      }
    }

    true
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_script_logic() {
    const TEST_SCRIPT_FILE: &str = "examples/test_scripts.ss";
    const TEST_SCRIPT_SRC: &str = include_str!("../examples/test_scripts.ss");

    let i = Interpreter::default_with_test_support();

    if let Err(e) = i.exec(TEST_SCRIPT_FILE, TEST_SCRIPT_SRC) {
      panic!("test script improperly written: {}", e);
    }
  }

  #[test]
  fn test_exec() {
    const INTEGER_SCRIPT: &str = "12345;";
    const STRING_SCRIPT: &str = "\"some string\";";
    const TRUE_SCRIPT: &str = "true;";
    const FALSE_SCRIPT: &str = "false;";
    const NIL_SCRIPT: &str = "nil;";

    let i = Interpreter::default();

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
      handle_result(res, i.exec("test", test))
    }
  }

  #[test]
  fn transfer_closure() {
    const CLOSURE_INIT_SCRIPT: &str = "|a, b| {
      a + b;
    };";
    const CLOSURE_CALL_SCRIPT: &str = "closure(1, 2);";
    let i1 = Interpreter::default();
    let mut i2 = Interpreter::default();

    let closure = i1.exec("test", CLOSURE_INIT_SCRIPT).unwrap();
    i2.set_var(&String::from("closure"), closure);

    handle_result(Value::Num(3.0), i2.exec("test", CLOSURE_CALL_SCRIPT));
  }

  fn handle_result(expected: Value, res: ExecResult) {
    match res {
      Ok(v) => assert_eq!(expected, v),
      Err(err) => println!("{}", err),
    }
  }
}
