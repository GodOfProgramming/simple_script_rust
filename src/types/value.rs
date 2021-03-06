use super::{Class, Function, Instance, New, ValueError};
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt::{self, Debug, Display};
use std::ops::{
  Add, AddAssign, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Not, Rem, RemAssign, Sub,
  SubAssign,
};

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
  pub fn truthy(&self) -> bool {
    if *self == Value::Nil {
      return false;
    }

    if let Value::Bool(b) = self {
      return *b;
    }

    true
  }

  pub fn len(&self) -> Option<usize> {
    if let Self::List(list) = self {
      Some(list.len())
    } else {
      None
    }
  }

  pub fn is_empty(&self) -> Option<bool> {
    if let Self::List(list) = self {
      Some(list.is_empty())
    } else {
      None
    }
  }

  pub fn index(&'_ self, idx: usize) -> Option<&'_ Self> {
    if let Self::List(list) = self {
      Some(&list[idx])
    } else {
      None
    }
  }

  pub fn index_mut(&'_ mut self, idx: usize) -> Option<&'_ mut Self> {
    if let Self::List(list) = self {
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

impl New<&str> for Value {
  fn new(item: &str) -> Self {
    Self::new(String::from(item))
  }
}

impl New<Values> for Value {
  fn new(item: Values) -> Self {
    Self::List(item)
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

impl ValueError<&str> for Value {
  fn new_err(err: &str) -> Self {
    Self::new_err(String::from(err))
  }
}

impl ValueError<String> for Value {
  fn new_err(err: String) -> Self {
    Self::Error(Box::new(Self::Str(err)))
  }
}

impl ValueError<Values> for Value {
  fn new_err(err: Values) -> Self {
    Self::Error(Box::new(Self::List(err)))
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
      Self::Num(a) => match other {
        Self::Num(b) => Self::Num(a + b),
        Self::Str(b) => Self::Str(format!("{}{}", a, b)),
        _ => Self::new_err(format!("cannot add {} and {}", a, other)),
      },
      Self::Str(a) => match other {
        Self::Num(b) => Self::Str(format!("{}{}", a, b)),
        Self::Str(b) => Self::Str(format!("{}{}", a, b)),
        _ => Self::new_err(format!("cannot add {} and {}", a, other)),
      },
      _ => Self::new_err(format!("cannot add {} and {}", self, other)),
    }
  }
}

impl AddAssign for Value {
  fn add_assign(&mut self, other: Self) {
    *self = self.clone() + other;
  }
}

impl Sub for Value {
  type Output = Self;

  fn sub(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Self::Num(a - b),
        _ => Self::new_err(format!("cannot sub {} and {}", a, other)),
      },
      _ => Self::new_err(format!("cannot sub {} and {}", self, other)),
    }
  }
}

impl SubAssign for Value {
  fn sub_assign(&mut self, other: Self) {
    *self = self.clone() - other;
  }
}

impl Mul for Value {
  type Output = Self;

  fn mul(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Self::Num(a * b),
        Self::Str(b) => {
          if a > 0.0 {
            Self::Str(b.repeat(a as usize))
          } else {
            Self::new_err(format!("cannot repeat a string {} times", b))
          }
        }
        _ => Self::new_err(format!("cannot multiply {} and {}", a, other)),
      },
      Self::Str(a) => match other {
        Self::Num(b) => {
          if b > 0.0 {
            Self::new(a.repeat(b as usize))
          } else {
            Self::new_err(format!("cannot repeat a string {} times", a))
          }
        }
        _ => Self::new_err(format!("cannot multiply {} and {}", a, other)),
      },
      _ => Self::new_err(format!("cannot multiply {} and {}", self, other)),
    }
  }
}

impl MulAssign for Value {
  fn mul_assign(&mut self, other: Self) {
    *self = self.clone() * other;
  }
}

impl Div for Value {
  type Output = Self;

  fn div(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Self::new(a / b),
        _ => Self::new_err(format!("cannot divide {} by {}", self, other)),
      },
      _ => Self::new_err(format!("cannot divide {} by {}", self, other)),
    }
  }
}

impl DivAssign for Value {
  fn div_assign(&mut self, other: Self) {
    *self = self.clone() / other;
  }
}

impl Rem for Value {
  type Output = Self;

  fn rem(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Self::new(a % b),
        _ => Self::new_err(format!("cannot modulus {} by {}", self, other)),
      },
      _ => Self::new_err(format!("cannot modulus {} by {}", self, other)),
    }
  }
}

impl RemAssign for Value {
  fn rem_assign(&mut self, other: Self) {
    *self = self.clone() % other;
  }
}

impl Not for Value {
  type Output = Self;

  fn not(self) -> Self::Output {
    Value::Bool(!self.truthy())
  }
}

impl Neg for Value {
  type Output = Self;

  fn neg(self) -> Self::Output {
    match self {
      Self::Nil => Self::new_err("cannot negate nil"),
      Self::Error(_) => Self::new_err("cannot negate an error"),
      Self::Bool(_) => Self::new_err("cannot negate a bool"),
      Self::Num(n) => Self::Num(-n),
      Self::Str(_) => Self::new_err("cannot negate a string"),
      Self::List(_) => Self::new_err("cannot negate a list"),
      Self::Callee(_) => Self::new_err("cannot negate a function"),
      Self::Class(_) => Self::new_err("cannot negate a class"),
      Self::Instance(i) => Self::new_err(format!("cannot negate a {}", i.instance_of)),
    }
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    match self {
      Self::Error(a) => {
        if let Self::Error(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Bool(a) => {
        if let Self::Bool(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Str(a) => {
        if let Self::Str(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Num(a) => {
        if let Self::Num(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::List(a) => {
        if let Self::List(b) = other {
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
      Self::Callee(_) => panic!("comparing functions is unimplemented"),
      Self::Class(a) => {
        if let Self::Class(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Instance(a) => {
        if let Self::Instance(b) = other {
          a == b
        } else {
          false
        }
      }
      Self::Nil => {
        matches!(other, Value::Nil)
      }
    }
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => {
          if a < b {
            Some(Ordering::Less)
          } else if a > b {
            Some(Ordering::Greater)
          } else if (a - b).abs() < f64::EPSILON {
            Some(Ordering::Equal)
          } else {
            None
          }
        }
        _ => None,
      },
      Self::Str(a) => match other {
        Self::Str(b) => Some(a.cmp(&b)),
        _ => None,
      },
      _ => None,
    }
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Nil => write!(f, "nil"),
      Self::Error(e) => write!(f, "{}", e),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Num(n) => write!(f, "{}", n),
      Self::Str(s) => write!(f, "{}", s),
      Self::List(l) => write!(f, "{}", l),
      Self::Callee(c) => write!(f, "{}", c),
      Self::Class(c) => write!(f, "<class {}>", c),
      Self::Instance(i) => write!(f, "<instance of {}>", i),
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::env::EnvRef;
  use crate::types::Airity;

  #[test]
  fn is_truthy() {
    let true_expectations = |t: Value| {
      assert!(t.truthy());
    };

    let false_expectations = |t: Value| {
      assert!(!t.truthy());
    };

    run_assertions(
      vec![true_expectations],
      vec![
        Value::new_err("can be any error type"),
        Value::new(true),
        Value::new(0.0),
        Value::new(1.0),
        Value::new(-1.0),
        Value::new("some string"),
        Value::new(Values::new(Vec::new())),
        // TODO
        // Value::new(Function::new_native(
        //   String::from("example"),
        //   Airity::Fixed(0),
        //   |_, _| Ok(Value::Nil),
        // )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );

    run_assertions(
      vec![false_expectations],
      vec![Value::Nil, Value::new(false)],
    );
  }

  fn run_assertions(funcs: Vec<fn(Value)>, values: Vec<Value>) {
    for func in funcs {
      let values = values.clone();
      for value in values {
        func(value);
      }
    }
  }

  #[test]
  fn can_add() {
    let x = Value::new(1.0);
    let y = Value::new(2.0);

    assert_eq!(x + y, Value::new(3.0));

    let x = Value::new("x");
    let y = Value::new("y");

    assert_eq!(x + y, Value::new("xy"));

    let x = Value::new(1.0);
    let y = Value::new("y");

    assert_eq!(x + y, Value::new("1y"));

    let x = Value::new("x");
    let y = Value::new(2.0);

    assert_eq!(x + y, Value::new("x2"));
  }

  #[test]
  fn cannot_add_invalid() {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num + t.clone(), Value::Error(_)));
      let num = Value::new(1.0);
      assert!(matches!(t + num, Value::Error(_)));
    };

    let assert_err_with_str = |t: Value| {
      let s = Value::new("a");
      assert!(matches!(s + t.clone(), Value::Error(_)));
      let s = Value::new("a");
      assert!(matches!(t + s, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_add_assign() {
    let mut x = Value::new(1.0);
    let y = Value::new(2.0);
    x += y;

    assert_eq!(x, Value::new(3.0));

    let mut x = Value::new("x");
    let y = Value::new("y");
    x += y;

    assert_eq!(x, Value::new("xy"));

    let mut x = Value::new(1.0);
    let y = Value::new("y");
    x += y;

    assert_eq!(x, Value::new("1y"));

    let mut x = Value::new("x");
    let y = Value::new(2.0);
    x += y;

    assert_eq!(x, Value::new("x2"));
  }

  #[test]
  fn cannot_add_assign_invalid() {
    let assert_err_with_num = |mut t: Value| {
      let mut num = Value::new(1.0);
      num += t.clone();
      assert!(matches!(num, Value::Error(_)));
      let num = Value::new(1.0);
      t += num;
      assert!(matches!(t, Value::Error(_)));
    };

    let assert_err_with_str = |mut t: Value| {
      let mut num = Value::new("a");
      num += t.clone();
      assert!(matches!(num, Value::Error(_)));
      let num = Value::new("a");
      t += num;
      assert!(matches!(t, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_sub() {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x - y, Value::new(1.0));
  }

  #[test]
  fn cannot_sub_invalid() {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num - t.clone(), Value::Error(_)));
      let num = Value::new(1.0);
      assert!(matches!(t - num, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_sub_assign() {
    let mut x = Value::new(3.0);
    let y = Value::new(2.0);
    x -= y;

    assert_eq!(x, Value::new(1.0));
  }

  #[test]
  fn cannot_sub_assign_invalid() {
    let assert_err_with_num = |mut t: Value| {
      let mut num = Value::new(1.0);
      num -= t.clone();
      assert!(matches!(num, Value::Error(_)));
      let num = Value::new(1.0);
      t -= num;
      assert!(matches!(t, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_mul() {
    let x = Value::new(2.0);
    let y = Value::new(3.0);

    assert_eq!(x * y, Value::new(6.0));

    let x = Value::new(2.0);
    let y = Value::new("a");

    assert_eq!(x * y, Value::new("aa"));

    let x = Value::new(2.0);
    let y = Value::new("a");

    assert_eq!(x * y, Value::new("aa"));
  }

  #[test]
  fn cannot_mul_invalid() {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num * t.clone(), Value::Error(_)));
      let num = Value::new(1.0);
      assert!(matches!(t * num, Value::Error(_)));
    };

    let assert_err_with_str = |t: Value| {
      let s = Value::new("a");
      assert!(matches!(s * t.clone(), Value::Error(_)));
      let s = Value::new("a");
      assert!(matches!(t * s, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );

    run_assertions(
      vec![assert_err_with_str],
      vec![Value::new(-1.0), Value::new("test")],
    );
  }

  #[test]
  fn can_mul_assign() {
    let mut x = Value::new(2.0);
    let y = Value::new(3.0);
    x *= y;

    assert_eq!(x, Value::new(6.0));

    let mut x = Value::new(2.0);
    let y = Value::new("a");
    x *= y;

    assert_eq!(x, Value::new("aa"));

    let mut x = Value::new(2.0);
    let y = Value::new("a");
    x *= y;

    assert_eq!(x, Value::new("aa"));
  }

  #[test]
  fn cannot_mul_assign_invalid() {
    let assert_err_with_num = |mut t: Value| {
      let mut num = Value::new(1.0);
      num *= t.clone();
      assert!(matches!(num, Value::Error(_)));
      let num = Value::new(1.0);
      t *= num;
      assert!(matches!(t, Value::Error(_)));
    };

    let assert_err_with_str = |mut t: Value| {
      let mut s = Value::new("a");
      s *= t.clone();
      assert!(matches!(s, Value::Error(_)));
      let s = Value::new("a");
      t *= s;
      assert!(matches!(t, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );

    run_assertions(
      vec![assert_err_with_str],
      vec![Value::new(-1.0), Value::new("test")],
    );
  }

  #[test]
  fn can_div() {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x / y, Value::new(1.5));
  }

  #[test]
  fn cannot_div_invalid() {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num / t.clone(), Value::Error(_)));
      let num = Value::new(1.0);
      assert!(matches!(t / num, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_div_assign() {
    let mut x = Value::new(3.0);
    let y = Value::new(2.0);
    x /= y;

    assert_eq!(x, Value::new(1.5));
  }

  #[test]
  fn cannot_div_assign_invalid() {
    let assert_err_with_num = |mut t: Value| {
      let mut num = Value::new(1.0);
      num /= t.clone();
      assert!(matches!(num, Value::Error(_)));
      let num = Value::new(1.0);
      t /= num;
      assert!(matches!(t, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_mod() {
    let x = Value::new(3.0);
    let y = Value::new(2.0);

    assert_eq!(x % y, Value::new(1.0));
  }

  #[test]
  fn cannot_mod_invalid() {
    let assert_err_with_num = |t: Value| {
      let num = Value::new(1.0);
      assert!(matches!(num % t.clone(), Value::Error(_)));
      let num = Value::new(1.0);
      assert!(matches!(t % num, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_mod_assign() {
    let mut x = Value::new(3.0);
    let y = Value::new(2.0);
    x %= y;

    assert_eq!(x, Value::new(1.0));
  }

  #[test]
  fn cannot_mod_assign_invalid() {
    let assert_err_with_num = |mut t: Value| {
      let mut num = Value::new(1.0);
      num %= t.clone();
      assert!(matches!(num, Value::Error(_)));
      let num = Value::new(1.0);
      t %= num;
      assert!(matches!(t, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn not_a_value_returns_opposite_truthiness() {
    let true_expectations = |t: Value| {
      assert_eq!(Value::new(true), !t);
    };

    let false_expectations = |t: Value| {
      assert_eq!(Value::new(false), !t);
    };

    run_assertions(vec![true_expectations], vec![Value::Nil, Value::new(false)]);

    run_assertions(
      vec![false_expectations],
      vec![
        Value::new_err("can be any error type"),
        Value::new(true),
        Value::new(0.0),
        Value::new(1.0),
        Value::new(-1.0),
        Value::new("some string"),
        Value::new(Values::new(Vec::new())),
        // TODO
        // Value::new(Function::new_native(
        //   String::from("example"),
        //   Airity::Fixed(0),
        //   |_, _| Ok(Value::Nil),
        // )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }

  #[test]
  fn can_negate() {
    let x = Value::new(1.0);
    assert_eq!(-x, Value::new(-1.0));
  }

  #[test]
  fn cannot_negate_invalid() {
    let assert_err = |t: Value| {
      assert!(matches!(-t, Value::Error(_)));
    };

    run_assertions(
      vec![assert_err],
      vec![
        Value::Nil,
        Value::new_err("test error"),
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
        Value::new(Function::new_native(
          String::from("example"),
          Airity::Fixed(0),
          |_, _| Ok(Value::Nil),
        )),
        Value::new(Class::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
        Value::new(Instance::new(
          String::from("example"),
          EnvRef::default(),
          EnvRef::default(),
        )),
      ],
    );
  }
}
