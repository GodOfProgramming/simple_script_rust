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
  Class { name: String, methods: EnvRef },
  Instance { instance_of: String, env: EnvRef },
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
      Value::Class { name, methods: _ } => write!(f, "<class {}>", name),
      Value::Instance {
        instance_of,
        env: _,
      } => write!(f, "<instance of {}>", instance_of),
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
      Value::Class {
        name: a,
        methods: _,
      } => {
        if let Value::Class {
          name: b,
          methods: _,
        } = other
        {
          a == b
        } else {
          false
        }
      }
      Value::Instance {
        instance_of: _,
        env: _,
      } => {
        // TODO call == method
        panic!("unimplemented");
      }
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

  pub fn new_native(name: String, airity: usize, func: NativeFn) -> Self {
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
    airity: &usize,
    func: &NativeFn,
    args: &[Value],
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
        if let Value::Instance {
          instance_of,
          env: instance_env,
        } = instance
        {
          env.define(
            self_ref.lexeme.clone(),
            Value::Instance {
              instance_of: instance_of.clone(),
              env: instance_env.snapshot(),
            },
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

    println!(">>>>>>\n{}<<<<<<", env);

    let res = Ok(match e.eval_block(&body, env.snapshot())? {
      StatementType::Regular(v) => v,
      StatementType::Return(v) => v,
    });

    println!(">>>>>>\n{}<<<<<<", env);

    res
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

pub trait Visitor<T, R> {
  fn visit(&mut self, _: &T) -> R;
}
