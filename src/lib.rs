mod ast;
mod builtin;
mod code;
mod env;
mod expr;
mod lex;
mod res;
mod stmt;
mod types;

use code::{Chunk, OpCode, VM};
use env::EnvRef;
use std::ffi::OsString;
use std::fmt::{self, Display};
use std::fs;
use std::io::{self, Write};
pub use types::{New, Value};

pub fn test() {
  let mut vm = VM::new();
  let mut chunk = Chunk::new(String::from("test"));
  chunk.write(OpCode::Constant { location: 0 }, 1);
  chunk.write(OpCode::Constant { location: 1 }, 1);
  chunk.add_constant(Value::Num(1.0));
  chunk.add_constant(Value::Num(2.0));
  chunk.write(OpCode::Return, 2);
  match vm.run(chunk) {
    Ok(_) => println!("terminated successfully"),
    Err(e) => println!("process exited with error: {:?}", e),
  }
}

#[derive(Debug)]
pub struct ScriptError {
  pub file: OsString,
  pub line: usize,
  pub msg: String,
}

impl Display for ScriptError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{} ({}): {}",
      self.file.to_string_lossy(),
      self.line,
      self.msg
    )
  }
}

pub type ExecResult = Result<Value, ScriptError>;

pub struct Interpreter {
  globals: EnvRef,
}

impl Default for Interpreter {
  fn default() -> Self {
    let mut globals = EnvRef::default();

    builtin::time::enable(&mut globals);
    builtin::meta::enable(&mut globals);
    builtin::sys::enable(&mut globals);

    Interpreter { globals }
  }
}

impl Interpreter {
  pub fn new_with_test_support() -> Self {
    let mut i = Interpreter::default();

    builtin::test::enable(&mut i.globals);

    i
  }

  pub fn get_var(&mut self, name: &str) -> Option<Value> {
    self.globals.get(name)
  }

  pub fn set_var(&mut self, name: &str, value: Value) {
    self.globals.define(name, value);
  }

  pub fn exec(&self, script_name: &OsString, src: &str) -> Result<Value, ScriptError> {
    let analysis = lex::analyze(script_name.clone(), src)?;
    let program = ast::parse(script_name.clone(), &analysis.tokens)?;
    let value = ast::exec(script_name.clone(), self.globals.snapshot(), program)?;
    Ok(value)
  }

  pub fn exec_file(&self, file: &OsString) -> Result<Value, ScriptError> {
    let src = fs::read_to_string(file).map_err(|err| ScriptError {
      file: file.into(),
      line: 0,
      msg: format!("could not read file: {}", err),
    })?;
    self.exec(file, &src)
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

      let analysis = match lex::analyze("ss".into(), &input) {
        Ok(a) => a,
        Err(err) => {
          // - 1 because analyze will read the \n from pressing enter
          println!(
            "{} ({}): {}",
            err.file.to_string_lossy(),
            err.line + line_number - 1,
            err.msg
          );
          continue;
        }
      };

      let program = match ast::parse("ss".into(), &analysis.tokens) {
        Ok(p) => p,
        Err(err) => {
          println!(
            "{} ({}): {}",
            err.file.to_string_lossy(),
            err.line + line_number - 1,
            err.msg
          );
          continue;
        }
      };

      match ast::exec("ss".into(), self.globals.snapshot(), program) {
        Ok(v) => {
          println!("=> {}", v);
          line_number += analysis.lines_analyzed;
        }
        Err(err) => println!(
          "{} ({}): {}",
          err.file.to_string_lossy(),
          err.line + line_number - 1,
          err.msg
        ),
      }
    }

    true
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_load_stmts() {
    const TEST_SCRIPT_FILE: &str = "examples/test_scripts.ss";
    const TEST_SCRIPT_SRC: &str = include_str!("../examples/test_scripts.ss");

    let i = Interpreter::new_with_test_support();

    if let Err(e) = i.exec(&TEST_SCRIPT_FILE.into(), TEST_SCRIPT_SRC) {
      panic!("test script improperly written: {}", e);
    }
  }

  #[test]
  fn test_values() {
    const INTEGER_SCRIPT: &str = "12345;";
    const STRING_SCRIPT: &str = "\"some string\";";
    const TRUE_SCRIPT: &str = "true;";
    const FALSE_SCRIPT: &str = "false;";
    const NIL_SCRIPT: &str = "nil;";

    let i = Interpreter::default();

    let results = vec![
      Value::new(12345.0),
      Value::new("some string"),
      Value::new(true),
      Value::new(false),
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
      handle_result(res, i.exec(&"test".into(), test))
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

    let closure = i1.exec(&"test".into(), CLOSURE_INIT_SCRIPT).unwrap();
    i2.set_var(&String::from("closure"), closure);

    handle_result(
      Value::new(3.0),
      i2.exec(&"test".into(), CLOSURE_CALL_SCRIPT),
    );
  }

  fn handle_result(expected: Value, res: ExecResult) {
    match res {
      Ok(v) => assert_eq!(expected, v),
      Err(err) => println!("{}", err),
    }
  }
}
