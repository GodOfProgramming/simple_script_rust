use crate::ast::{Evaluator, StatementType};
use crate::env::EnvRef;
use crate::lex::Token;
use crate::stmt::Stmt;
use crate::ScriptError;
use std::fmt::{self, Debug, Display};
use std::ops::{
  Add, AddAssign, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Not, RangeInclusive, Rem,
  RemAssign, Sub, SubAssign,
};
use std::rc::Rc;

#[derive(Clone)]
pub struct Class {
  pub name: String,
  pub methods: EnvRef,
}

impl PartialEq for Class {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)
  }
}

#[derive(Clone)]
pub struct Instance {
  pub instance_of: String,
  pub methods: EnvRef,
  pub members: EnvRef,
}

impl PartialEq for Instance {
  fn eq(&self, other: &Self) -> bool {
    self.instance_of == other.instance_of && self.members == other.members
  }
}

impl Display for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.instance_of)
  }
}

pub trait ValueError<T> {
  fn new_err(err: T) -> Self;
}

pub trait New<T> {
  fn new(item: T) -> Self;
}

#[derive(Clone)]
pub enum Value {
  Nil,
  Error(Box<Value>),
  Bool(bool),
  Num(f64),
  Str(String),
  List(Values),
  Callee(Function),
  Class(Class),
  Instance(Instance),
}

impl Value {
  pub fn len(&self) -> Option<usize> {
    if let Value::List(list) = self {
      Some(list.len())
    } else {
      None
    }
  }

  pub fn is_empty(&self) -> Option<bool> {
    if let Value::List(list) = self {
      Some(list.is_empty())
    } else {
      None
    }
  }

  pub fn index(&'_ self, idx: usize) -> Option<&'_ Value> {
    if let Value::List(list) = self {
      Some(&list[idx])
    } else {
      None
    }
  }

  pub fn index_mut(&'_ mut self, idx: usize) -> Option<&'_ mut Value> {
    if let Value::List(list) = self {
      Some(&mut list[idx])
    } else {
      None
    }
  }
}

impl New<bool> for Value {
  fn new(item: bool) -> Self {
    Self::Bool(item)
  }
}

impl New<f64> for Value {
  fn new(item: f64) -> Self {
    Self::Num(item)
  }
}

impl New<String> for Value {
  fn new(item: String) -> Self {
    Self::Str(item)
  }
}

impl New<Function> for Value {
  fn new(item: Function) -> Self {
    Self::Callee(item)
  }
}

impl New<Class> for Value {
  fn new(item: Class) -> Self {
    Self::Class(item)
  }
}

impl New<Instance> for Value {
  fn new(item: Instance) -> Self {
    Self::Instance(item)
  }
}

impl ValueError<bool> for Value {
  fn new_err(err: bool) -> Self {
    Self::Error(Box::new(Self::Bool(err)))
  }
}

impl ValueError<f64> for Value {
  fn new_err(err: f64) -> Self {
    Self::Error(Box::new(Self::Num(err)))
  }
}

impl ValueError<String> for Value {
  fn new_err(err: String) -> Self {
    Self::Error(Box::new(Self::Str(err)))
  }
}

impl ValueError<Function> for Value {
  fn new_err(err: Function) -> Self {
    Self::Error(Box::new(Self::Callee(err)))
  }
}

impl ValueError<Class> for Value {
  fn new_err(err: Class) -> Self {
    Self::Error(Box::new(Self::Class(err)))
  }
}

impl ValueError<Instance> for Value {
  fn new_err(err: Instance) -> Self {
    Self::Error(Box::new(Self::Instance(err)))
  }
}

impl Add for Value {
  type Output = Self;
  fn add(self, other: Self) -> Self {
    match self {
      Value::Num(a) => match other {
        Value::Num(b) => Value::Num(a + b),
        Value::Str(b) => Value::Str(format!("{}{}", a, b)),
        _ => Value::new(format!("cannot add {} and {}", a, other)),
      },
      Value::Str(a) => match other {
        Value::Num(b) => Value::Str(format!("{}{}", a, b)),
        Value::Str(b) => Value::Str(format!("{}{}", a, b)),
        _ => Value::Error(format!("cannot add {} and {}", a, other)),
      },
      _ => Value::Error(format!("cannot add {} and {}", self, other)),
    }
  }
}

impl AddAssign for Value {
  fn add_assign(&mut self, other: Value) {
    *self = match self {
      Value::Num(a) => match other {
        Value::Num(b) => Value::Num(*a + b),
        Value::Str(b) => Value::Str(format!("{}{}", a, b)),
        _ => Value::Error(format!("cannot add {} and {}", a, other)),
      },
      Value::Str(a) => match other {
        Value::Num(b) => Value::Str(format!("{}{}", a, b)),
        Value::Str(b) => Value::Str(format!("{}{}", a, b)),
        _ => Value::Error(format!("cannot add {} and {}", a, other)),
      },
      _ => Value::Error(format!("cannot add {} and {}", self, other)),
    };
  }
}

impl Sub for Value {
  type Output = Self;

  fn sub(self, other: Self) -> Self::Output {
    match self {
      Value::Num(a) => match other {
        Value::Num(b) => Value::Num(a - b),
        _ => Value::new(format!("cannot sub {} and {}", a, other)),
      },
      Value::Str(a) => match other {
        Value::Num(b) => Value::Str(format!("{}{}", a, b)),
        _ => Value::Error(format!("cannot sub {} and {}", a, other)),
      },
      _ => Value::Error(format!("cannot sub {} and {}", self, other)),
    }
  }
}

impl SubAssign for Value {
  fn sub_assign(&mut self, other: Value) {
    *self = match self {
      Value::Num(a) => match other {
        Value::Num(b) => Value::Num(*a - b),
        _ => Value::Error(format!("cannot sub {} and {}", a, other)),
      },
      Value::Str(a) => match other {
        Value::Num(b) => Value::Str(format!("{}{}", a, b)),
        _ => Value::Error(format!("cannot sub {} and {}", a, other)),
      },
      _ => Value::Error(format!("cannot sub {} and {}", self, other)),
    };
  }
}

impl Mul for Value {
  type Output = Self;

  fn mul(self, other: Self) -> Self::Output {
    match self {
      Value::Num(a) => match other {
        Value::Num(b) => Value::Num(a * b),
        Value::Str(b) => {
          if a > 0.0 {
            Value::Str(b.repeat(a as usize))
          } else {
            Value::new_err(format!("cannot repeat a string {} times", b))
          }
        }
        _ => Value::new(format!("cannot add {} and {}", a, other)),
      },
      Value::Str(a) => match other {
        Value::Num(b) => {
          if b > 0.0 {
            Value::Str(a.repeat(b as usize))
          } else {
            Value::new_err(format!("cannot repeat a string {} times", a))
          }
        }
        Value::Str(b) => Value::new_err(String::from("cannot multiply two strings")),
        _ => Value::Error(format!("cannot add {} and {}", a, other)),
      },
      _ => Value::Error(format!("cannot add {} and {}", self, other)),
    }
  }
}

impl Neg for Value {
  type Output = Self;

  fn neg(self) -> Self::Output {
    match self {
      Self::Nil => Self::Nil,
      Self::Error(_) => Self::new_err(String::from("cannot negate an error")),
      Self::Bool(b) => Self::Bool(!b),
      Self::Num(n) => Self::Num(-n),
      Self::Str(_) => Self::new_err(String::from("cannot negate a string")),
      Self::List(_) => Self::new_err(String::from("cannot negate a list")),
      Self::Callee(_) => Self::new_err(String::from("cannot negate a function")),
      Self::Class(_) => Self::new_err(String::from("cannot negate a class")),
      Self::Instance(_) => panic!("unimplemented"),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Value) -> bool {
    match self {
      Value::Error(a) => {
        if let Value::Error(b) = other {
          a == b
        } else {
          false
        }
      }
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
      Value::Callee(_) => panic!("comparing functions is unimplemented"),
      Value::Class(a) => {
        if let Value::Class(b) = other {
          a == b
        } else {
          false
        }
      }
      Value::Instance(a) => {
        if let Value::Instance(b) = other {
          a == b
        } else {
          false
        }
      }
      Value::Nil => {
        matches!(other, Value::Nil)
      }
    }
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Nil => write!(f, "nil"),
      Value::Error(e) => write!(f, "{}", e),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Num(n) => write!(f, "{}", n),
      Value::Str(s) => write!(f, "{}", s),
      Value::List(l) => write!(f, "{}", l),
      Value::Callee(c) => write!(f, "{}", c),
      Value::Class(c) => write!(f, "<class {}>", c),
      Value::Instance(i) => write!(f, "<instance of {}>", i),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Display::fmt(self, f)
  }
}

#[derive(Clone)]
pub struct Values(Vec<Value>);

impl Values {
  pub fn new(values: Vec<Value>) -> Self {
    Self(values)
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

impl Index<usize> for Values {
  type Output = Value;

  fn index(&self, idx: usize) -> &Self::Output {
    &self.0[idx]
  }
}

impl IndexMut<usize> for Values {
  fn index_mut(&mut self, idx: usize) -> &mut Value {
    &mut self.0[idx]
  }
}

impl IntoIterator for Values {
  type Item = Value;
  type IntoIter = std::vec::IntoIter<Self::Item>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl Display for Values {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.iter().fold(Ok(()), |result, value| {
      result.and_then(|_| writeln!(f, "{}", value))
    })
  }
}

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

pub trait Visitor<T, R> {
  fn visit(&mut self, _: &T) -> R;
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn can_add_integer_values() {
    let x = Value::Num(1.0);
    let y = Value::Num(2.0);

    assert_eq!(x + y, Value::Num(3.0));
  }
}
