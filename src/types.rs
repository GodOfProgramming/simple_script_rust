use crate::ast::AstErr;
use crate::ast::{Evaluator, StatementType};
use crate::env::Env;
use crate::expr::ClosureExpr;
use crate::stmt::FunctionStmt;
use std::cell::RefCell;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
  Nil,
  Bool(bool),
  Str(String),
  Num(f64),
  List(Values),
  Callee(Rc<dyn Callable>),
}

impl Value {
  pub fn from(v: &Value) -> Value {
    v.clone()
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Nil => write!(f, "nil"),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Num(n) => write!(f, "{}", n),
      Value::Str(s) => write!(f, "{}", s),
      Value::List(l) => write!(f, "{}", l),
      Value::Callee(c) => write!(f, "{}", c),
    }
  }
}

#[derive(Clone)]
pub struct Values(Vec<Value>);

impl Values {
  pub fn new(values: Vec<Value>) -> Self {
    Self(values)
  }
}

impl Display for Values {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.iter().fold(Ok(()), |result, value| {
      result.and_then(|_| writeln!(f, "{}", value))
    })
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(self, f)
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Value) -> bool {
    match self {
      Value::Bool(a) => {
        if let Value::Bool(b) = other {
          a == b
        } else {
          false
        }
      }
      Value::Str(a) => {
        if let Value::Str(b) = other {
          a == b
        } else {
          false
        }
      }
      Value::Num(a) => {
        if let Value::Num(b) = other {
          a == b
        } else {
          false
        }
      }
      Value::List(a) => {
        if let Value::List(b) = other {
          let a = &a.0;
          let b = &b.0;

          if a.len() == b.len() {
            for it in a.iter().zip(b.iter()) {
              let (ai, bi) = it;
              if ai != bi {
                return false;
              }
            }

            true
          } else {
            false
          }
        } else {
          false
        }
      }
      Value::Callee(_) => false, // TODO figure this out
      Value::Nil => {
        if let Value::Nil = other {
          true
        } else {
          false
        }
      }
    }
  }
}

pub type CallResult = Result<Value, CallErr>;

pub struct CallErr {
  pub file: String,
  pub line: usize,
  pub msg: String,
}

impl From<AstErr> for CallErr {
  fn from(err: AstErr) -> Self {
    CallErr {
      file: err.file,
      line: err.line,
      msg: err.msg,
    }
  }
}

pub trait Callable: Display {
  fn call(&self, evaluator: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult;
}

pub type NativeResult = Result<Value, String>;

pub struct NativeFunction<T>
where
  T: Fn(Vec<Value>) -> NativeResult,
{
  airity: usize,
  func: T,
}

impl<T> NativeFunction<T>
where
  T: Fn(Vec<Value>) -> NativeResult,
{
  pub fn new(airity: usize, func: T) -> Self {
    Self { airity, func }
  }
}

impl<T> Display for NativeFunction<T>
where
  T: Fn(Vec<Value>) -> NativeResult,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<native function>")
  }
}

impl<T> Callable for NativeFunction<T>
where
  T: Fn(Vec<Value>) -> NativeResult,
{
  fn call(&self, e: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult {
    if self.airity < args.len() {
      return Err(CallErr {
        file: e.file.clone(),
        line,
        msg: format!(
          "too many arguments, expected {}, got {}",
          self.airity,
          args.len()
        ),
      });
    }

    if self.airity > args.len() {
      return Err(CallErr {
        file: e.file.clone(),
        line,
        msg: format!(
          "too few arguments, expected {}, got {}",
          self.airity,
          args.len(),
        ),
      });
    }

    match (self.func)(args) {
      Ok(v) => Ok(v),
      Err(msg) => Err(CallErr {
        file: e.file.clone(),
        line,
        msg,
      }),
    }
  }
}

pub struct ScriptFunction {
  pub func: FunctionStmt,
}

impl ScriptFunction {
  pub fn new(func: FunctionStmt) -> ScriptFunction {
    ScriptFunction { func }
  }
}

impl Display for ScriptFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<fn {}>", self.func.name)
  }
}

impl Callable for ScriptFunction {
  fn call(&self, e: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult {
    let func = &self.func;
    if func.params.len() < args.len() {
      return Err(CallErr {
        file: e.file.clone(),
        line,
        msg: format!(
          "too many arguments, expected {}, got {}",
          func.params.len(),
          args.len()
        ),
      });
    }

    if func.params.len() > args.len() {
      return Err(CallErr {
        file: e.file.clone(),
        line,
        msg: format!(
          "too few arguments, expected {}, got {}",
          func.params.len(),
          args.len(),
        ),
      });
    }

    let env = Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(&e.env))));

    for (param, arg) in func.params.iter().zip(args.iter()) {
      env.borrow_mut().define(param.lexeme.clone(), arg.clone())
    }

    Ok(match e.eval_block(&func.body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }
}

pub struct Closure {
  pub exec: ClosureExpr,
  env: Rc<RefCell<Env>>,
}

impl Closure {
  pub fn new(exec: ClosureExpr, env: Rc<RefCell<Env>>) -> Self {
    Self { exec, env }
  }
}

impl Display for Closure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<closure>")
  }
}

impl Callable for Closure {
  fn call(&self, e: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult {
    let func = &self.exec;
    if func.params.len() < args.len() {
      return Err(CallErr {
        file: e.file.clone(),
        line,
        msg: format!(
          "too many arguments, expected {}, got {}",
          func.params.len(),
          args.len()
        ),
      });
    }

    if func.params.len() > args.len() {
      return Err(CallErr {
        file: e.file.clone(),
        line,
        msg: format!(
          "too few arguments, expected {}, got {}",
          func.params.len(),
          args.len(),
        ),
      });
    }

    let env = Rc::new(RefCell::new(Env::new_with_enclosing(Rc::clone(&self.env))));

    for (param, arg) in func.params.iter().zip(args.iter()) {
      env.borrow_mut().define(param.lexeme.clone(), arg.clone())
    }

    Ok(match e.eval_block(&func.body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }
}
