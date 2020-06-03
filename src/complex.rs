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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
  airity: usize,
}

impl Function {
  pub fn new(airity: usize) -> Function {
    Function { airity }
  }
}

impl Callable for Function {
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
