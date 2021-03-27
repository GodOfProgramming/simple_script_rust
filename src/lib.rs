use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::collections::BTreeMap;
use std::fmt::{self, Debug, Display};
use std::ops::{
  Add, AddAssign, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Not, Rem, RemAssign, Sub,
  SubAssign,
};

#[derive(Debug)]
enum OpCode {
  /**
   * No operation instruction
   */
  NoOp,
  /**
   *  Looks up a constant value. The index is specified by the modifying bits
   */
  Const,
  /**
   * Pushes a nil value on to the stack
   */
  Nil,
  /**
   * Pushes a true value on to the stack
   */
  True,
  /**
   * Pushes a false value on to the stack
   */
  False,
  /**
   * Pops a value off the stack
   */
  Pop,
  /**
   * Pops N values off the stack. N is specified by the modifying bits
   */
  PopN,
  /**
   * Looks up a local variable. The index in the stack is specified by the modifying bits
   */
  LookupLocal,
  /**
   * Assigns a value to the local variable. The value comes off the top of the stack
   */
  AssignLocal,
  /**
   * Looks up a global variable. The name is stored in the constant list. The index to then name is specified by the modifying bits
   */
  LookupGlobal,
  /**
   * Defines a new global variable. The name is stored in the constant list. The index to then name is specified by the modifying bits
   */
  DefineGlobal,
  /**
   * Assigns a value to the global variable. The value comes off the top of the stack
   */
  AssignGlobal,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Equal,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  NotEqual,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Greater,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  GreaterEqual,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Less,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  LessEqual,
  /**
   * Pops a value off the stack, and compars it with the peeked value, pushing the new value on
   */
  Check,
  /**
   * Pops two values off the stack, calculates the sum, then pushes the result back on
   */
  Add,
  /**
   * Pops two values off the stack, calculates the difference, then pushes the result back on
   */
  Sub,
  /**
   * Pops two values off the stack, calculates the product, then pushes the result back on
   */
  Mul,
  /**
   * Pops two values off the stack, calculates the quotient, then pushes the result back on
   */
  Div,
  /**
   * Pops two values off the stack, calculates the remainder, then pushes the result back on
   */
  Mod,
  /**
   * Pops a value off the stack, inverts its truthy value, then pushes that back on
   */
  Not,
  /**
   * Pops a value off the stack, inverts its numerical value, then pushes that back on
   */
  Negate,
  /**
   * Pops a value off the stack and prints it to the screen
   */
  Print,
  /**
   * Swaps the top two values on the stack.
   */
  Swap,
  /**
   * Moves, or rather shifts, the top value in the stack down N slots, specified by the modifying bits.
   */
  Move,
  /**
   * Jumps to a code location indicated by the modifying bits
   */
  Jump,
  /**
   * Jumps to a code location indicated by the modifying bits
   */
  JumpIfFalse,
  /**
   * Jumps the instruction pointer backwards N instructions. N specified by the modifying bits
   */
  Loop,
  /**
   * Peeks at the stack, if the top value is true short circuits to the instruction pointed to by the modifying bit
   */
  Or,
  /**
   * Peeks at the stack, if the top value is false short circuits to the instruction pointed to by the modifying bit
   */
  And,
  /**
   * Pushes the stack pointer onto the stack
   */
  PushSp,
  /** Calls the instruction on the stack. Number of arguments is specified by the modifying bits */
  Call,
  /** TODO */
  Return,
  /** TODO */
  End,
}

type Instructions = Vec<OpCode>;

struct VPU;

struct Context {
  instructions: Instructions,
  ip: usize,
}

#[derive(Debug)]
enum Token {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Semicolon,
  Plus,
  Minus,
  Asterisk,
  Slash,
  Modulus,

  // One or two character tokens.
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Arrow,

  // Literals.
  Identifier(String),
  String(String),
  Number(f64),

  // Keywords.
  And,
  Break,
  Class,
  Cont,
  Else,
  End,
  False,
  For,
  Fn,
  If,
  Let,
  Load,
  LoadRel,
  Loop,
  Match,
  Nil,
  Or,
  Print,
  Return,
  True,
  While,
  Eof,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Token::Identifier(i) => write!(f, "Identifier ({})", i),
      Token::String(s) => write!(f, "String ({})", s),
      Token::Number(n) => write!(f, "Number ({})", n),
      _ => write!(f, "{:?}", self),
    }
  }
}

trait New<T> {
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

type ValueOpResult = Result<Value, String>;

impl Add for Value {
  type Output = ValueOpResult;
  fn add(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a + b)),
        Self::Str(b) => Ok(Self::Str(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      Self::Str(a) => match other {
        Self::Num(b) => Ok(Self::Str(format!("{}{}", a, b))),
        Self::Str(b) => Ok(Self::Str(format!("{}{}", a, b))),
        _ => Err(format!("cannot add {} and {}", a, other)),
      },
      _ => Err(format!("cannot add {} and {}", self, other)),
    }
  }
}

impl Sub for Value {
  type Output = ValueOpResult;
  fn sub(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a - b)),
        _ => Err(format!("cannot sub {} and {}", a, other)),
      },
      _ => Err(format!("cannot sub {} and {}", self, other)),
    }
  }
}

impl Mul for Value {
  type Output = ValueOpResult;
  fn mul(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::Num(a * b)),
        Self::Str(b) => {
          if a > 0.0 {
            Ok(Self::Str(b.repeat(a as usize)))
          } else {
            Err(format!("cannot repeat a string {} times", b))
          }
        }
        _ => Err(format!("cannot multiply {} and {}", a, other)),
      },
      Self::Str(a) => match other {
        Self::Num(b) => {
          if b > 0.0 {
            Ok(Self::new(a.repeat(b as usize)))
          } else {
            Err(format!("cannot repeat a string {} times", a))
          }
        }
        _ => Err(format!("cannot multiply {} and {}", a, other)),
      },
      _ => Err(format!("cannot multiply {} and {}", self, other)),
    }
  }
}

impl Div for Value {
  type Output = ValueOpResult;
  fn div(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::new(a / b)),
        _ => Err(format!("cannot divide {} by {}", self, other)),
      },
      _ => Err(format!("cannot divide {} by {}", self, other)),
    }
  }
}

impl Rem for Value {
  type Output = ValueOpResult;
  fn rem(self, other: Self) -> Self::Output {
    match self {
      Self::Num(a) => match other {
        Self::Num(b) => Ok(Self::new(a % b)),
        _ => Err(format!("cannot modulus {} by {}", self, other)),
      },
      _ => Err(format!("cannot modulus {} by {}", self, other)),
    }
  }
}

impl Not for Value {
  type Output = Self;
  fn not(self) -> Self::Output {
    Value::Bool(!self.truthy())
  }
}

impl Neg for Value {
  type Output = ValueOpResult;
  fn neg(self) -> Self::Output {
    match self {
      Self::Nil => Err(String::from("cannot negate nil")),
      Self::Error(_) => Err(String::from("cannot negate an error")),
      Self::Bool(_) => Err(String::from("cannot negate a bool")),
      Self::Num(n) => Ok(Self::Num(-n)),
      Self::Str(_) => Err(String::from("cannot negate a string")),
      Self::List(_) => Err(String::from("cannot negate a list")),
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
        Value::new(true),
        Value::new(0.0),
        Value::new(1.0),
        Value::new(-1.0),
        Value::new("some string"),
        Value::new(Values::new(Vec::new())),
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
      assert!(matches!(num + t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t + num, Err(_)));
    };

    let assert_err_with_str = |t: Value| {
      let s = Value::new("a");
      assert!(matches!(s + t.clone(), Ok(_)));
      let s = Value::new("a");
      assert!(matches!(t + s, Ok(_)));
    };

    run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new(Values(Vec::new())),
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
      assert!(matches!(num - t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t - num, Err(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
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
      assert!(matches!(num * t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t * num, Err(_)));
    };

    let assert_err_with_str = |t: Value| {
      let s = Value::new("a");
      assert!(matches!(s * t.clone(), Err(_)));
      let s = Value::new("a");
      assert!(matches!(t * s, Err(_)));
    };

    run_assertions(
      vec![assert_err_with_num, assert_err_with_str],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new(Values(Vec::new())),
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
      assert!(matches!(num / t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t / num, Err(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
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
      assert!(matches!(num % t.clone(), Err(_)));
      let num = Value::new(1.0);
      assert!(matches!(t % num, Err(_)));
    };

    run_assertions(
      vec![assert_err_with_num],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
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
        Value::new(true),
        Value::new(0.0),
        Value::new(1.0),
        Value::new(-1.0),
        Value::new("some string"),
        Value::new(Values::new(Vec::new())),
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
      assert!(matches!(-t, Err(_)));
    };

    run_assertions(
      vec![assert_err],
      vec![
        Value::Nil,
        Value::new(true),
        Value::new(false),
        Value::new("test"),
        Value::new(Values(Vec::new())),
      ],
    );
  }
}
