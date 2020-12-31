use code::{scanner::Scanner, Chunk, Error, OpCode};
use std::io::{self, Write};
use types::Value;
use util::ScriptError;

macro_rules! is_debug {
  () => {
    cfg!(debug_assertions)
  };
}

pub type VMResult = std::result::Result<(), Error>;

pub struct VM;

impl Default for VM {
  fn default() -> Self {
    Self
  }
}

impl VM {
  pub fn run_script(&mut self, script: &str) -> VMResult {
    self.compile(script)
  }

  fn compile(&mut self, src: &str) -> VMResult {
    let mut scanner = Scanner::default();
    let tokens = scanner.scan(src).map_err(|err| Error::Compile(err))?;
    let mut last_line: Option<usize> = None;

    for token in tokens {
      if let Some(line) = last_line {
        if token.line != line {
          print!("{:04} ", token.line);
          last_line = Some(token.line);
        } else {
          print!("   | ")
        }
      } else {
        print!("{:04} ", token.line);
        last_line = Some(token.line);
      }

      println!("{}", token);
    }
    Ok(())
  }

  pub fn run(&mut self, chunk: Chunk) -> VMResult {
    let mut _sp: usize = 0;
    let mut stack: Vec<Value> = Vec::new();

    for (ip, instruction) in chunk.code.iter().enumerate() {
      if is_debug!() {
        for slot in &stack {
          print!("[ {} ]", slot);
        }
        if !stack.is_empty() {
          println!();
        }

        chunk.print_instruction(ip, instruction);
      }
      match instruction {
        OpCode::Constant { location } => {
          let constant = &chunk.constants[*location];
          stack.push(constant.clone());
        }
        OpCode::Add => {
          let res = self.operate_on(&chunk, ip, "add", &mut stack, |a, b| a + b)?;
          stack.push(res);
        }
        OpCode::Subtract => {
          let res = self.operate_on(&chunk, ip, "subtract", &mut stack, |a, b| a - b)?;
          stack.push(res);
        }
        OpCode::Multiply => {
          let res = self.operate_on(&chunk, ip, "multiply", &mut stack, |a, b| a * b)?;
          stack.push(res);
        }
        OpCode::Divide => {
          let res = self.operate_on(&chunk, ip, "divide", &mut stack, |a, b| a / b)?;
          stack.push(res);
        }
        OpCode::Negate => {
          if let Some(v) = stack.pop() {
            stack.push(-v);
          } else {
            return Err(Error::Runtime(ScriptError {
              line: chunk.line_at(ip),
              msg: String::from("tried to negate a void value"),
            })); // tried to negate a void value
          }
        }
        OpCode::Return => {
          if let Some(v) = stack.pop() {
            println!("{}", v);
          }
          return Ok(());
        }
      }
    }
    Ok(())
  }

  pub fn cli(&mut self) -> bool {
    let mut input = String::new();
    let mut exit = false;
    let mut line_number = 1;

    while !exit {
      input.clear();
      print!("ss(main):{}> ", line_number);
      if let Err(err) = io::stdout().flush() {
        println!("{}", err);
        return false;
      }

      if let Err(err) = io::stdin().read_line(&mut input) {
        println!("{}", err);
        return false;
      }

      if input == "exit" {
        exit = true;
      }

      if let Err(e) = self.run_script(&input) {
        println!("{}", e);
      } else {
        line_number += 1;
      }
    }

    true
  }

  fn operate_on(
    &mut self,
    chunk: &Chunk,
    offset: usize,
    operator: &str,
    stack: &mut Vec<Value>,
    f: fn(a: Value, b: Value) -> Value,
  ) -> Result<Value, Error> {
    if let Some(a) = stack.pop() {
      if let Some(b) = stack.pop() {
        Ok(f(a, b))
      } else {
        Err(Error::Runtime(ScriptError {
          line: chunk.line_at(offset),
          msg: format!("cannot {} with {} and void", operator, a),
        }))
      }
    } else {
      Err(Error::Runtime(ScriptError {
        line: chunk.line_at(offset),
        msg: format!("cannot {} with void", operator),
      }))
    }
  }
}

mod types {
  use super::util::{New, ValueError};
  use std::cmp::{Ordering, PartialEq, PartialOrd};
  use std::fmt::{self, Debug, Display};
  use std::ops::{
    Add, AddAssign, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Not, Rem, RemAssign, Sub,
    SubAssign,
  };

  #[derive(Clone)]
  pub enum ErrorValue {
    Bool(bool),
    Num(f64),
    Str(String),
    List(Values),
  }

  impl New<bool> for ErrorValue {
    fn new(b: bool) -> Self {
      Self::Bool(b)
    }
  }

  impl New<f64> for ErrorValue {
    fn new(n: f64) -> Self {
      Self::Num(n)
    }
  }

  impl New<String> for ErrorValue {
    fn new(s: String) -> Self {
      Self::Str(s)
    }
  }

  impl New<Values> for ErrorValue {
    fn new(v: Values) -> Self {
      Self::List(v)
    }
  }

  impl PartialEq for ErrorValue {
    fn eq(&self, other: &ErrorValue) -> bool {
      match self {
        Self::Bool(a) => {
          if let Self::Bool(b) = other {
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
        Self::Str(a) => {
          if let Self::Str(b) = other {
            a == b
          } else {
            false
          }
        }
        Self::List(a) => {
          if let Self::List(b) = other {
            a == b
          } else {
            false
          }
        }
      }
    }
  }

  impl Display for ErrorValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
        Self::Bool(b) => write!(f, "{}", b),
        Self::Num(n) => write!(f, "{}", n),
        Self::Str(s) => write!(f, "{}", s),
        Self::List(l) => write!(f, "{}", l),
      }
    }
  }

  #[derive(Clone)]
  pub enum Value {
    Nil,
    Error(ErrorValue),
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

  impl ValueError<bool> for Value {
    fn new_err(err: bool) -> Self {
      Self::Error(ErrorValue::Bool(err))
    }
  }

  impl ValueError<f64> for Value {
    fn new_err(err: f64) -> Self {
      Self::Error(ErrorValue::new(err))
    }
  }

  impl ValueError<&str> for Value {
    fn new_err(err: &str) -> Self {
      Self::new_err(String::from(err))
    }
  }

  impl ValueError<String> for Value {
    fn new_err(err: String) -> Self {
      Self::Error(ErrorValue::new(err))
    }
  }

  impl ValueError<Values> for Value {
    fn new_err(err: Values) -> Self {
      Self::Error(ErrorValue::new(err))
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

  impl PartialEq for Values {
    fn eq(&self, other: &Self) -> bool {
      self.0 == other.0
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
          Value::new_err("can be any error type"),
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
        ],
      );
    }
  }
}

mod code {
  use crate::{ScriptError, Value};
  use std::fmt::{self, Display};
  use std::ops::Index;

  #[derive(Debug)]
  pub enum OpCode {
    Constant { location: usize },
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
  }

  #[derive(Debug)]
  pub enum Error {
    Compile(ScriptError),
    Runtime(ScriptError),
  }

  impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
        Self::Compile(se) => write!(f, "compile error: {}", se),
        Self::Runtime(se) => write!(f, "runtime error: {}", se),
      }
    }
  }

  pub struct Chunk {
    _name: String,
    pub code: Vec<OpCode>,
    pub constants: ValueArray,
    lines: Vec<usize>,
    last_line: usize,
    instructions_on_line: usize,
  }

  impl Chunk {
    pub fn new(name: String) -> Self {
      Self {
        _name: name,
        code: Vec::new(),
        lines: Vec::new(),
        last_line: 0,
        instructions_on_line: 0,
        constants: ValueArray::new(),
      }
    }

    pub fn write(&mut self, oc: OpCode, line: usize) {
      self.code.push(oc);
      self.add_line(line);
    }

    // returns the location of the added constant
    pub fn add_constant(&mut self, value: Value) -> usize {
      self.constants.write(value)
    }

    // increments the current number of instructions on a line
    // or publishes the number and resets the count
    fn add_line(&mut self, line: usize) {
      if self.last_line == line {
        // same line number
        self.instructions_on_line += 1;
      } else {
        self.lines.push(self.instructions_on_line);
        self.last_line = line;
        self.instructions_on_line = 1; // current instruction
      }
    }

    // extracts the line at the given instruction offset
    pub fn line_at(&self, offset: usize) -> usize {
      let mut accum = 0;
      for (line, num_instns) in self.lines.iter().enumerate() {
        if accum + num_instns > offset {
          return line;
        } else {
          accum += num_instns;
        }
      }
      self.lines.len()
    }

    pub fn print_instruction(&self, offset: usize, instruction: &OpCode) {
      print!("{:04} ", offset);
      if offset > 0 && self.line_at(offset) == self.line_at(offset - 1) {
        print!("   | ");
      } else {
        print!("{:04} ", self.line_at(offset));
      }
      match instruction {
        OpCode::Return => println!("RETURN"),
        OpCode::Negate => println!("NEGATE"),
        OpCode::Add => println!("ADD"),
        OpCode::Subtract => println!("SUBTRACT"),
        OpCode::Multiply => println!("MULTIPLY"),
        OpCode::Divide => println!("DIVIDE"),
        OpCode::Constant { location } => println!(
          "{:<16} {:04} {}",
          "CONSTANT", location, self.constants[*location]
        ),
      }
    }

    fn _print_header(&self) {
      println!("== {} ==", self._name);
      println!("{:<4} {:<4} {:<16} {:<4}", "off", "line", "opcode", "extra");
    }

    fn _print(&self) {
      self._print_header();
      self.code.iter().enumerate().for_each(|(offset, inst)| {
        self.print_instruction(offset, inst);
      });
    }
  }

  #[derive(Clone)]
  pub struct ValueArray {
    values: Vec<Value>,
  }

  impl ValueArray {
    pub fn new() -> Self {
      Self { values: Vec::new() }
    }

    // returns the location of the added constant
    pub fn write(&mut self, value: Value) -> usize {
      self.values.push(value);
      self.values.len() - 1
    }
  }

  impl Index<usize> for ValueArray {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
      &self.values[index]
    }
  }

  pub mod scanner {
    use super::ScriptError;
    use crate::util::New;
    use std::fmt::{self, Display};
    use std::iter::{Peekable, Skip};

    #[derive(Debug, PartialEq)]
    pub enum TokenKind {
      // Single-character tokens.
      LeftParen,
      RightParen,
      LeftBrace,
      RightBrace,
      Comma,
      Minus,
      Plus,
      Slash,
      Asterisk,
      Semicolon,
      BackSlash,
      Conditional,
      Colon,
      Pipe,

      // One or two character tokens.
      Not,
      NotEq,
      Equal,
      EqEq,
      GreaterThan,
      GreaterEq,
      LessThan,
      LessEq,
      Dot,
      Range,

      Identifier,

      // Literals.
      StringLiteral,
      NumberLiteral,

      // Keywords.
      And,
      Bool,
      Class,
      Else,
      Error,
      False,
      Fn,
      For,
      If,
      Let,
      List,
      Nil,
      Number,
      Or,
      Print,
      Return,
      String,
      True,
      While,

      EOF,
    }

    #[derive(Debug, PartialEq)]
    pub struct Token<'lexeme> {
      pub kind: TokenKind,
      pub lexeme: &'lexeme str,
      pub line: usize,
    }

    impl<'lexeme> Token<'lexeme> {
      fn new(kind: TokenKind, lexeme: &'lexeme str, line: usize) -> Token {
        Token { kind, lexeme, line }
      }
    }

    impl Display for Token<'_> {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lexeme)
      }
    }

    pub struct Scanner {
      start: usize,
      current: usize,
      line: usize,
    }

    impl Default for Scanner {
      fn default() -> Self {
        Self {
          start: 0,
          current: 0,
          line: 1,
        }
      }
    }

    type SrcIter<'src> = std::iter::Peekable<std::str::Chars<'src>>;

    impl Scanner {
      pub fn scan<'src>(&'src mut self, src: &'src str) -> Result<Vec<Token<'src>>, ScriptError> {
        let mut chars = src.chars().peekable();
        let mut tokens = Vec::new();
        loop {
          self.skip_nontokens(&mut chars);
          self.start = self.current;

          if let Some(c) = self.advance(&mut chars) {
            tokens.push(match c {
              '(' => self.make_token(src, TokenKind::LeftParen),
              ')' => self.make_token(src, TokenKind::RightParen),
              '{' => self.make_token(src, TokenKind::LeftBrace),
              '}' => self.make_token(src, TokenKind::RightBrace),
              ';' => self.make_token(src, TokenKind::Semicolon),
              ',' => self.make_token(src, TokenKind::Comma),
              '.' => {
                if self.advance_if_matches('.', &mut chars) {
                  self.make_token(src, TokenKind::Range)
                } else {
                  self.make_token(src, TokenKind::Dot)
                }
              }
              '+' => self.make_token(src, TokenKind::Plus),
              '-' => self.make_token(src, TokenKind::Minus),
              '*' => self.make_token(src, TokenKind::Asterisk),
              '/' => self.make_token(src, TokenKind::Slash),
              '!' => {
                if self.advance_if_matches('=', &mut chars) {
                  self.make_token(src, TokenKind::NotEq)
                } else {
                  self.make_token(src, TokenKind::Not)
                }
              }
              '=' => {
                if self.advance_if_matches('=', &mut chars) {
                  self.make_token(src, TokenKind::EqEq)
                } else {
                  self.make_token(src, TokenKind::Equal)
                }
              }
              '<' => {
                if self.advance_if_matches('=', &mut chars) {
                  self.make_token(src, TokenKind::LessEq)
                } else {
                  self.make_token(src, TokenKind::LessThan)
                }
              }
              '>' => {
                if self.advance_if_matches('=', &mut chars) {
                  self.make_token(src, TokenKind::GreaterEq)
                } else {
                  self.make_token(src, TokenKind::GreaterThan)
                }
              }
              '"' => self.make_string(src, &mut chars)?,
              _ if Scanner::is_alpha(c) => self.make_ident(src, &mut chars),
              _ if Scanner::is_digit(c) => self.make_number(src, &mut chars)?,
              c => {
                return Err(ScriptError {
                  line: self.line,
                  msg: format!("unexpected character: {}", c),
                })
              }
            });
          } else {
            tokens.push(Token::new(TokenKind::EOF, "EOF", self.line));
            break;
          }
        }

        Ok(tokens)
      }

      fn current_lexeme<'src>(&self, src: &'src str) -> &'src str {
        &src[self.start..self.current]
      }

      fn make_token<'src>(&self, src: &'src str, kind: TokenKind) -> Token<'src> {
        Token::new(kind, self.current_lexeme(src), self.line)
      }

      fn advance(&mut self, chars: &mut SrcIter) -> Option<char> {
        self.current += 1;
        chars.next()
      }

      fn advance_if_matches(&mut self, expected: char, chars: &mut SrcIter) -> bool {
        match chars.peek() {
          Some(c) => {
            if *c == expected {
              self.advance(chars);
              true
            } else {
              false
            }
          }
          None => false,
        }
      }

      fn skip_nontokens(&mut self, chars: &mut SrcIter) {
        while let Some(c) = chars.peek() {
          match c {
            ' ' | '\r' | '\t' => {
              self.advance(chars);
            }
            '\n' => {
              self.line += 1;
              self.advance(chars);
            }
            '#' => {
              while let Some(c) = self.advance(chars) {
                if c == '\n' {
                  self.line += 1;
                  break;
                }
              }
            }
            _ => return,
          }
        }
      }

      fn make_string<'src>(
        &mut self,
        src: &'src str,
        chars: &mut SrcIter,
      ) -> Result<Token<'src>, ScriptError> {
        while let Some(c) = self.advance(chars) {
          if c == '\n' {
            self.line += 1;
          } else if c == '"' {
            return Ok(self.make_token(src, TokenKind::StringLiteral));
          }
        }

        Err(ScriptError {
          line: self.line,
          msg: String::from("unterminated string"),
        })
      }

      fn make_number<'src>(
        &mut self,
        src: &'src str,
        chars: &mut SrcIter,
      ) -> Result<Token<'src>, ScriptError> {
        while let Some(c) = chars.peek() {
          if !Scanner::is_digit(*c) {
            break;
          }
          self.advance(chars);
        }

        if let Some(c) = chars.peek() {
          if *c == '.' {
            if let Some(c) = Scanner::peek_next(chars, 1) {
              if Self::is_digit(c) {
                self.advance(chars);
                while let Some(c) = chars.peek() {
                  if !Self::is_digit(*c) {
                    break;
                  }
                  self.advance(chars);
                }
              }
            }
          }
        }

        Ok(self.make_token(src, TokenKind::NumberLiteral))
      }

      fn make_ident<'src>(&mut self, src: &'src str, chars: &mut SrcIter) -> Token<'src> {
        while let Some(c) = chars.peek() {
          if !Self::is_alpha(*c) && !Self::is_digit(*c) {
            break;
          }

          self.advance(chars);
        }

        self.make_token(src, self.ident(src))
      }

      fn ident(&self, src: &str) -> TokenKind {
        let lex = self.current_lexeme(src);
        let mut chars = lex.chars();
        match chars.next().unwrap() {
          'a' => self.check_keyword(src, 1, "nd", TokenKind::And),
          'b' => self.check_keyword(src, 1, "ool", TokenKind::Bool),
          'c' => self.check_keyword(src, 1, "lass", TokenKind::Class),
          'e' => match chars.next() {
            Some(c) => match c {
              'l' => self.check_keyword(src, 2, "se", TokenKind::Else),
              'r' => self.check_keyword(src, 2, "ror", TokenKind::Error),
              _ => TokenKind::Identifier,
            },
            None => TokenKind::Identifier,
          },
          'f' => match chars.next() {
            Some(c) => match c {
              'a' => self.check_keyword(src, 2, "lse", TokenKind::False),
              'n' => self.check_keyword(src, 2, "", TokenKind::Fn),
              'o' => self.check_keyword(src, 2, "r", TokenKind::For),
              _ => TokenKind::Identifier,
            },
            None => TokenKind::Identifier,
          },
          'i' => self.check_keyword(src, 1, "f", TokenKind::If),
          'l' => match chars.next() {
            Some(c) => match c {
              'e' => self.check_keyword(src, 2, "t", TokenKind::Let),
              'i' => self.check_keyword(src, 2, "st", TokenKind::List),
              _ => TokenKind::Identifier,
            },
            None => TokenKind::Identifier,
          },
          'n' => match chars.next() {
            Some(c) => match c {
              'i' => self.check_keyword(src, 2, "l", TokenKind::Nil),
              'u' => self.check_keyword(src, 2, "mber", TokenKind::Number),
              _ => TokenKind::Identifier,
            },
            None => TokenKind::Identifier,
          },
          'o' => self.check_keyword(src, 1, "r", TokenKind::Or),
          'p' => self.check_keyword(src, 1, "rint", TokenKind::Print),
          'r' => self.check_keyword(src, 1, "eturn", TokenKind::Return),
          's' => self.check_keyword(src, 1, "tring", TokenKind::String),
          't' => self.check_keyword(src, 1, "rue", TokenKind::True),
          'w' => self.check_keyword(src, 1, "hile", TokenKind::While),
          _ => TokenKind::Identifier,
        }
      }

      fn check_keyword(
        &self,
        src: &str,
        cmp_start: usize,
        rest: &str,
        keyword: TokenKind,
      ) -> TokenKind {
        if &self.current_lexeme(src)[cmp_start..] == rest {
          keyword
        } else {
          TokenKind::Identifier
        }
      }

      fn is_digit(c: char) -> bool {
        c >= '0' && c <= '9'
      }

      fn is_alpha(c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
      }

      fn peek_next(chars: &mut SrcIter, skips: usize) -> Option<char> {
        chars.clone().nth(skips)
      }
    }

    #[cfg(test)]
    mod tests {
      use super::*;

      macro_rules! gen_scan_test {
        ($func:ident, $str:expr, [$($x:expr),+ $(,)?]) => {
          #[test]
          fn $func()
          {
            let cmp_tokens = |expected: &[Token], actual: &[Token]| {
              assert_eq!(expected.len(), actual.len());

              for (a, b) in expected.iter().zip(actual.iter()) {
                assert_eq!(a, b);
              }
            };
            let mut s = Scanner::default();
            let actual = s.scan($str).unwrap();
            let expected = vec![$($x),+];
            cmp_tokens(&expected, &actual);
          }
        };
        ($func:ident, [$($x:expr),+ $(,)?]) => {
          gen_scan_test!($func, stringify!($func), [$($x),+]);
        };
      }

      gen_scan_test!(
        parens,
        "()",
        [
          Token::new(TokenKind::LeftParen, "(", 1),
          Token::new(TokenKind::RightParen, ")", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        braces,
        "{}",
        [
          Token::new(TokenKind::LeftBrace, "{", 1),
          Token::new(TokenKind::RightBrace, "}", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        semicolon,
        ";",
        [
          Token::new(TokenKind::Semicolon, ";", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        comma,
        ",",
        [
          Token::new(TokenKind::Comma, ",", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        dot,
        ".",
        [
          Token::new(TokenKind::Dot, ".", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        range,
        "..",
        [
          Token::new(TokenKind::Range, "..", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        plus,
        "+",
        [
          Token::new(TokenKind::Plus, "+", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        minus,
        "-",
        [
          Token::new(TokenKind::Minus, "-", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        asterisk,
        "*",
        [
          Token::new(TokenKind::Asterisk, "*", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        slash,
        "/",
        [
          Token::new(TokenKind::Slash, "/", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        not,
        "!",
        [
          Token::new(TokenKind::Not, "!", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        not_eq,
        "!=",
        [
          Token::new(TokenKind::NotEq, "!=", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        equal,
        "=",
        [
          Token::new(TokenKind::Equal, "=", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        eqeq,
        "==",
        [
          Token::new(TokenKind::EqEq, "==", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        greater_than,
        ">",
        [
          Token::new(TokenKind::GreaterThan, ">", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        greater_equal,
        ">=",
        [
          Token::new(TokenKind::GreaterEq, ">=", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        less_than,
        "<",
        [
          Token::new(TokenKind::LessThan, "<", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        less_equal,
        "<=",
        [
          Token::new(TokenKind::LessEq, "<=", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        ident,
        "abcxyz _abcxyz ABCXYZ",
        [
          Token::new(TokenKind::Identifier, "abcxyz", 1),
          Token::new(TokenKind::Identifier, "_abcxyz", 1),
          Token::new(TokenKind::Identifier, "ABCXYZ", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        string_literal,
        "\"some string\"",
        [
          Token::new(TokenKind::StringLiteral, "\"some string\"", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        number_literal,
        "0 1 2 3 4 5 6 7 8 9 10 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0",
        [
          Token::new(TokenKind::NumberLiteral, "0", 1),
          Token::new(TokenKind::NumberLiteral, "1", 1),
          Token::new(TokenKind::NumberLiteral, "2", 1),
          Token::new(TokenKind::NumberLiteral, "3", 1),
          Token::new(TokenKind::NumberLiteral, "4", 1),
          Token::new(TokenKind::NumberLiteral, "5", 1),
          Token::new(TokenKind::NumberLiteral, "6", 1),
          Token::new(TokenKind::NumberLiteral, "7", 1),
          Token::new(TokenKind::NumberLiteral, "8", 1),
          Token::new(TokenKind::NumberLiteral, "9", 1),
          Token::new(TokenKind::NumberLiteral, "10", 1),
          Token::new(TokenKind::NumberLiteral, "0.0", 1),
          Token::new(TokenKind::NumberLiteral, "1.0", 1),
          Token::new(TokenKind::NumberLiteral, "2.0", 1),
          Token::new(TokenKind::NumberLiteral, "3.0", 1),
          Token::new(TokenKind::NumberLiteral, "4.0", 1),
          Token::new(TokenKind::NumberLiteral, "5.0", 1),
          Token::new(TokenKind::NumberLiteral, "6.0", 1),
          Token::new(TokenKind::NumberLiteral, "7.0", 1),
          Token::new(TokenKind::NumberLiteral, "8.0", 1),
          Token::new(TokenKind::NumberLiteral, "9.0", 1),
          Token::new(TokenKind::NumberLiteral, "10.0", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        and,
        [
          Token::new(TokenKind::And, "and", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );

      gen_scan_test!(
        keyword_bool,
        "bool",
        [
          Token::new(TokenKind::Bool, "bool", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        class,
        [
          Token::new(TokenKind::Class, "class", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_else,
        "else",
        [
          Token::new(TokenKind::Else, "else", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        error,
        [
          Token::new(TokenKind::Error, "error", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_false,
        "false",
        [
          Token::new(TokenKind::False, "false", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_fn,
        "fn",
        [
          Token::new(TokenKind::Fn, "fn", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_for,
        "for",
        [
          Token::new(TokenKind::For, "for", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_if,
        "if",
        [
          Token::new(TokenKind::If, "if", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_let,
        "let",
        [
          Token::new(TokenKind::Let, "let", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        list,
        [
          Token::new(TokenKind::List, "list", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        nil,
        [
          Token::new(TokenKind::Nil, "nil", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        number,
        [
          Token::new(TokenKind::Number, "number", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        or,
        [
          Token::new(TokenKind::Or, "or", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        print,
        [
          Token::new(TokenKind::Print, "print", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_return,
        "return",
        [
          Token::new(TokenKind::Return, "return", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        string,
        [
          Token::new(TokenKind::String, "string", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_true,
        "true",
        [
          Token::new(TokenKind::True, "true", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        keyword_while,
        "while",
        [
          Token::new(TokenKind::While, "while", 1),
          Token::new(TokenKind::EOF, "EOF", 1)
        ]
      );

      gen_scan_test!(
        math,
        "let y = m * x + b;",
        [
          Token::new(TokenKind::Let, "let", 1),
          Token::new(TokenKind::Identifier, "y", 1),
          Token::new(TokenKind::Equal, "=", 1),
          Token::new(TokenKind::Identifier, "m", 1),
          Token::new(TokenKind::Asterisk, "*", 1),
          Token::new(TokenKind::Identifier, "x", 1),
          Token::new(TokenKind::Plus, "+", 1),
          Token::new(TokenKind::Identifier, "b", 1),
          Token::new(TokenKind::Semicolon, ";", 1),
          Token::new(TokenKind::EOF, "EOF", 1),
        ]
      );
    }
  }
}

mod util {
  use std::fmt::{self, Display};

  #[derive(Debug)]
  pub struct ScriptError {
    pub line: usize,
    pub msg: String,
  }

  impl Display for ScriptError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "({}): {}", self.line, self.msg)
    }
  }

  pub trait New<T> {
    fn new(item: T) -> Self;
  }

  pub trait ValueError<T> {
    fn new_err(err: T) -> Self;
  }
}
