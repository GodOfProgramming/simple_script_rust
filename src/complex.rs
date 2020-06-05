use crate::ast::Evaluator;
use crate::types::Value;

pub type CallResult = Result<Value, CallErr>;

pub struct CallErr {
  pub msg: String,
  pub line: usize,
}

pub trait Callable {
  fn call(&self, evaluator: &Evaluator, args: Vec<Value>) -> CallResult;
}

pub struct NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  airity: usize,
  func: T,
}

impl<T> NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  pub fn new(airity: usize, func: T) -> NativeFunction<T> {
    NativeFunction { airity, func }
  }
}

impl<T> Callable for NativeFunction<T>
where
  T: Fn(Vec<Value>) -> CallResult,
{
  fn call(&self, evaluator: &Evaluator, args: Vec<Value>) -> CallResult {
    if self.airity < args.len() {
      return Err(CallErr {
        msg: format!(
          "too many arguments, expected {}, got {}",
          self.airity,
          args.len()
        ),
        line: 0, // TODO
      });
    }

    if self.airity > args.len() {
      return Err(CallErr {
        msg: format!(
          "too few arguments, expected {}, got {}",
          self.airity,
          args.len(),
        ),
        line: 0, // TODO
      });
    }

    (self.func)(args)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UserFunction {
  airity: usize,
}

impl UserFunction {
  pub fn new(airity: usize) -> UserFunction {
    UserFunction { airity }
  }
}

impl Callable for UserFunction {
  fn call(&self, evaluator: &Evaluator, args: Vec<Value>) -> CallResult {
    if self.airity < args.len() {
      return Err(CallErr {
        msg: format!(
          "too many arguments, expected {}, got {}",
          self.airity,
          args.len()
        ),
        line: 0, // TODO
      });
    }

    if self.airity > args.len() {
      return Err(CallErr {
        msg: format!(
          "too few arguments, expected {}, got {}",
          self.airity,
          args.len(),
        ),
        line: 0, // TODO
      });
    }

    Ok(Value::Nil)
  }
}
