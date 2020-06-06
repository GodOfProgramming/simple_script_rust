use crate::ast::{self, AstErr};
use crate::env::{Env, EnvRef};
use crate::lex::{self, LexicalErr};
use crate::types::Value;
use crate::builtin;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Mutex;

pub struct ExecResult {
  pub value: Value,
  pub lines: usize,
}

pub struct ExecErr {
  pub msg: String,
  pub line: usize,
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
