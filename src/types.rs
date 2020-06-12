use crate::complex::Callable;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
  Nil,
  Bool(bool),
  Str(String),
  Num(f64),
  List(Vec<Value>),
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
      Value::List(l) => {
        // TODO better formatting
        for v in l.iter() {
          write!(f, "{}", v)?;
        }
        Ok(())
      }
      Value::Callee(c) => write!(f, "{}", c),
    }
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
