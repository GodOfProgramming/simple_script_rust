use super::{Instance, Value};
use crate::ast::{Evaluator, StatementType};
use crate::env::EnvRef;
use crate::lex::Token;
use crate::stmt::Stmt;
use crate::ScriptError;
use std::fmt::{self, Display};
use std::ops::RangeInclusive;
use std::rc::Rc;

pub type NativeFnResult = Result<Value, String>;
pub type NativeFn = fn(EnvRef, &[Value]) -> NativeFnResult;

#[derive(Clone)]
pub enum Airity {
  Fixed(usize),
  Range(RangeInclusive<usize>),
}

#[derive(Clone)]
pub enum Function {
  Native {
    name: String,
    airity: Airity,
    func: NativeFn,
  },
  Script {
    name: String,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
    env: EnvRef,
  },
  Closure {
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
    env: EnvRef,
  },
  Method {
    name: String,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
    env: EnvRef,
  },
}

pub type CallResult = Result<Value, ScriptError>;

impl Function {
  pub fn call(&self, evaluator: &mut Evaluator, args: Vec<Value>, line: usize) -> CallResult {
    match self {
      Function::Native {
        name: _,
        airity,
        func,
      } => Function::call_native_fn(evaluator, line, airity, func, &args),
      Function::Script {
        name: _,
        params,
        body,
        env,
      } => Function::call_script_fn(
        evaluator,
        line,
        params,
        body,
        args,
        EnvRef::new_with_enclosing(env.snapshot()),
      ),
      Function::Closure { params, body, env } => Function::call_closure_fn(
        evaluator,
        line,
        params,
        body,
        args,
        EnvRef::new_with_enclosing(env.snapshot()),
      ),
      Function::Method {
        name: _,
        params,
        body,
        env,
      } => Function::call_method(
        evaluator,
        line,
        params,
        body,
        args,
        EnvRef::new_with_enclosing(env.snapshot()),
      ),
    }
  }

  pub fn new_native(name: String, airity: Airity, func: NativeFn) -> Self {
    Self::Native { name, airity, func }
  }

  pub fn new_script(
    name: String,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
    env: EnvRef,
  ) -> Self {
    Self::Script {
      name,
      params,
      body,
      env,
    }
  }

  pub fn new_closure(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>, env: EnvRef) -> Self {
    Self::Closure { params, body, env }
  }

  pub fn new_method(
    name: String,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Stmt>>,
    env: EnvRef,
  ) -> Self {
    Self::Method {
      name,
      params,
      body,
      env,
    }
  }

  fn call_native_fn(
    e: &mut Evaluator,
    line: usize,
    airity: &Airity,
    func: &NativeFn,
    args: &[Value],
  ) -> CallResult {
    match airity {
      Airity::Fixed(len) => {
        if *len < args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!("too many arguments, expected {}, got {}", len, args.len()),
          });
        }
        if *len > args.len() {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!("too few arguments, expected {}, got {}", len, args.len(),),
          });
        }
      }
      Airity::Range(range) => {
        if !range.contains(&args.len()) {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: format!(
              "invalid number of arguments, expected range {:?}, got {}",
              range,
              args.len(),
            ),
          });
        }
      }
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
    e: &mut Evaluator,
    line: usize,
    params: &[Token],
    body: &[Stmt],
    args: Vec<Value>,
    mut env: EnvRef,
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

    for (param, arg) in params.iter().zip(args.iter()) {
      env.define(param.lexeme.clone(), arg.clone());
    }

    Ok(match e.eval_block(&body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }

  fn call_closure_fn(
    e: &mut Evaluator,
    line: usize,
    params: &[Token],
    body: &[Stmt],
    args: Vec<Value>,
    mut env: EnvRef,
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

    for (param, arg) in params.iter().zip(args.iter()) {
      env.define(param.lexeme.clone(), arg.clone());
    }

    Ok(match e.eval_block(&body, env)? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }

  fn call_method(
    e: &mut Evaluator,
    line: usize,
    params: &[Token],
    body: &[Stmt],
    args: Vec<Value>,
    mut env: EnvRef,
  ) -> CallResult {
    if params.is_empty() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: String::from("need at least one parameter for reference to self"),
      });
    }

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

    if params.len() - 1 > args.len() {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: format!(
          "too few arguments, expected {}, got {}",
          params.len() - 1,
          args.len(),
        ),
      });
    }

    if let Some(self_ref) = params.first() {
      if let Some(instance) = &e.last_object {
        if let Value::Instance(instance) = instance {
          env.define(
            self_ref.lexeme.clone(),
            Value::Instance(Instance {
              instance_of: instance.instance_of.clone(),
              methods: instance.methods.snapshot(),
              members: instance.members.snapshot(),
            }),
          );
        } else {
          return Err(ScriptError {
            file: e.file.clone(),
            line,
            msg: String::from("calling method on non-objects is not allowed"),
          });
        }
      } else {
        return Err(ScriptError {
          file: e.file.clone(),
          line,
          msg: String::from("method called on void space"),
        });
      }
    } else {
      return Err(ScriptError {
        file: e.file.clone(),
        line,
        msg: String::from("need at least one parameter for reference to self"),
      });
    }

    for (param, arg) in params.iter().skip(1).zip(args.iter()) {
      env.define(param.lexeme.clone(), arg.clone());
    }

    Ok(match e.eval_block(&body, env.snapshot())? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    })
  }
}

impl Display for Function {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Native {
        name,
        airity: _,
        func: _,
      } => write!(f, "<nf {}>", name),
      Self::Script {
        name,
        params: _,
        body: _,
        env: _,
      } => write!(f, "<fn {}>", name),
      Self::Closure {
        params: _,
        body: _,
        env: _,
      } => write!(f, "<closure>"),
      Self::Method {
        name,
        params: _,
        body: _,
        env: _,
      } => write!(f, "<m {}>", name),
    }
  }
}
