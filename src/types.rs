use crate::ast::{Evaluator, StatementType};
use crate::env::EnvRef;
use crate::lex::Token;
use crate::stmt::Stmt;
use crate::ScriptError;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
  Nil,
  Bool(bool),
  Str(String),
  Num(f64),
  List(Values),
  Callee(Function),
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
      Value::Str(s) => write!(f, "\"{}\"", s),
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
        matches!(other, Value::Nil)
      }
    }
  }
}

type NativeFnResult = Result<Value, String>;
type NativeFn = fn(EnvRef, &[Value]) -> NativeFnResult;

#[derive(Clone)]
pub enum Function {
  Native {
    name: String,
    airity: usize,
    func: NativeFn,
  },
  Script {
    name: String,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
  },
  Closure {
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
    env: EnvRef,
  },
}

pub type CallResult = Result<Value, ScriptError>;

pub trait Callable {}

impl Function {
  pub fn call(&self, evaluator: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult {
    match self {
      Function::Native { name: _, airity, func } => {
        Function::call_native_fn(airity, func, evaluator, &args, line)
      }
      Function::Script { name: _, params, body } => {
        Function::call_script_fn(params, body, evaluator, args, line)
      }
      Function::Closure { params, body, env } => {
        Function::call_closure_fn(params, body, env, evaluator, args, line)
      }
    }
  }

  pub fn new_native(name: String, airity: usize, func: NativeFn) -> Self {
    Self::Native { name, airity, func }
  }

  pub fn new_script(name: String, params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>) -> Self {
    Self::Script { name, params, body }
  }

  pub fn new_closure(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>, env: EnvRef) -> Self {
    Self::Closure { params, body, env }
  }

  fn call_native_fn(
    airity: &usize,
    func: &NativeFn,
    e: &mut Evaluator,
    args: &[Value],
    line: usize,
  ) -> CallResult {
    if *airity < args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!(
          "too many arguments, expected {}, got {}",
          airity,
          args.len()
        ),
      });
    }

    if *airity > args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!("too few arguments, expected {}, got {}", airity, args.len(),),
      });
    }

    match (func)(e.env.snapshot(), args) {
      Ok(v) => Ok(v),
      Err(msg) => Err(ScriptError {
        file: e.file.clone(),
        line,
        msg,
      }),
    }
  }

  fn call_script_fn(
    params: &[Token],
    body: &[Stmt],
    e: &mut Evaluator,
    args: Vec<Value>,
    line: usize,
  ) -> CallResult {
    if params.len() < args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!(
          "too many arguments, expected {}, got {}",
          params.len(),
          args.len()
        ),
      });
    }

    if params.len() > args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!(
          "too few arguments, expected {}, got {}",
          params.len(),
          args.len(),
        ),
      });
    }

    let mut env = EnvRef::new_with_enclosing(e.env.snapshot());

    for (param, arg) in params.iter().zip(args.iter()) {
      env.define(param.lexeme.clone(), arg.clone())
    }

    Ok(match e.eval_block(&body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }

  fn call_closure_fn(
    params: &[Token],
    body: &[Stmt],
    env: &EnvRef,
    e: &mut Evaluator,
    args: Vec<Value>,
    line: usize,
  ) -> CallResult {
    if params.len() < args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!(
          "too many arguments, expected {}, got {}",
          params.len(),
          args.len()
        ),
      });
    }

    if params.len() > args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!(
          "too few arguments, expected {}, got {}",
          params.len(),
          args.len(),
        ),
      });
    }

    let mut env = EnvRef::new_with_enclosing(env.snapshot());

    for (param, arg) in params.iter().zip(args.iter()) {
      env.define(param.lexeme.clone(), arg.clone())
    }

    Ok(match e.eval_block(&body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }
}

impl Display for Function {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Native { name, airity: _, func: _ } => {
        write!(f, "<nf {}>", name)
      }
      Self::Script { name, params: _, body: _ } => {
        write!(f, "<fn {}>", name)
      }
      _ => {
        write!(f, "<closure>")
      }
    }
  }
}

pub trait Visitor<T, R> {
  fn visit(&mut self, _: &T) -> R;
}
