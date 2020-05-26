use crate::ast::{self, Printer};
use crate::expr::{Binary, Expr, Grouping, Literal, Unary, Visitor};
use crate::lex::{self, TokenType, Value};
use std::io::{self, Write};

pub struct Interpreter {}

impl Interpreter {
  pub fn new() -> Interpreter {
    Interpreter {}
  }

  pub fn run_interactive(&self) -> io::Result<()> {
    let mut input = String::new();
    let exit = false;
    let mut line_number = 0;

    while !exit {
      print!("ss(main):{}> ", line_number);
      io::stdout().flush()?;
      io::stdin().read_line(&mut input)?;
      match self.exec(&input) {
        Ok(lines_executed) => line_number += lines_executed,
        Err((err_line, msg)) => println!("{}: {}", msg, line_number + err_line),
      }
      input.clear();
    }

    Ok(())
  }

  pub fn exec(&self, src: &str) -> Result<usize, (usize, String)> {
    let (lines_executed, tokens) = match lex::analyze(src) {
      Ok(tuple) => tuple,
      Err(line) => {
        return Err((line, String::from("analyze error")));
      }
    };

    let expr = match ast::parse(&tokens) {
      Ok(ast) => ast,
      Err(msg) => return Err((0, format!("parse error: {}", msg))),
    };

    let mut evaluator = Evaluator::new();

    let value = match evaluator.eval(&expr) {
      Ok(v) => v,
      Err(msg) => return Err((0, msg)),
    };

    println!("{:?}", value);

    Ok(lines_executed)
  }
}

type EvalResult = Result<Value, String>;

struct Evaluator;

impl Evaluator {
  fn new() -> Evaluator {
    Evaluator {}
  }

  fn eval(&mut self, expr: &Box<dyn Expr<EvalResult>>) -> EvalResult {
    expr.accept(self)
  }

  fn is_truthy(&mut self, v: Value) -> Value {
    if v == Value::Nil {
      return Value::Bool(false);
    }

    if let Value::Bool(_) = v {
      return v;
    }

    Value::Bool(true)
  }
}

impl Visitor<'_, EvalResult> for Evaluator {
  fn visit_binary_expr(&mut self, e: &Binary<EvalResult>) -> EvalResult {
    let left = self.eval(&e.left)?;
    let right = self.eval(&e.right)?;

    // value comparison
    if e.operator.token_type == TokenType::EqEq {
      return Ok(Value::Bool(left == right));
    }

    if e.operator.token_type == TokenType::ExEq {
      return Ok(Value::Bool(left != right));
    }

    // number arithmetic and comparison
    if let Value::Num(l) = left {
      if let Value::Num(r) = right {
        return match e.operator.token_type {
          TokenType::Plus => Ok(Value::Num(l + r)),
          TokenType::Minus => Ok(Value::Num(l - r)),
          TokenType::Slash => Ok(Value::Num(l / r)),
          TokenType::Asterisk => Ok(Value::Num(l * r)),
          TokenType::GreaterThan => Ok(Value::Bool(l > r)),
          TokenType::GreaterEq => Ok(Value::Bool(l >= r)),
          TokenType::LessThan => Ok(Value::Bool(l < r)),
          TokenType::LessEq => Ok(Value::Bool(l <= r)),
          _ => Err(format!("Invalid operator ({:?}) for {} and {}", e.operator, l, r)),
        };
      }
    }

    // string concatenation
    if e.operator.token_type == TokenType::Plus {
      if let Value::Str(l) = left.clone() {
        if let Value::Str(r) = right {
          return Ok(Value::Str(format!("{}{}", l, r)));
        } else if let Value::Num(r) = right {
          return Ok(Value::Str(format!("{}{}", l, r)));
        }
      } else if let Value::Num(l) = left {
        if let Value::Str(r) = right {
          return Ok(Value::Str(format!("{}{}", l, r)));
        }
      }
    }

    Err(format!(
      "Combination of {:?} and {:?} not allowed with {:?}",
      left, right, e.operator
    ))
  }

  fn visit_grouping_expr(&mut self, e: &Grouping<EvalResult>) -> EvalResult {
    self.eval(&e.expression)
  }

  fn visit_literal_expr(&mut self, e: &Literal<EvalResult>) -> EvalResult {
    Ok(e.value.clone())
  }

  fn visit_unary_expr(&mut self, e: &Unary<EvalResult>) -> EvalResult {
    let right = self.eval(&e.right)?;

    match e.operator.token_type {
      TokenType::Exclamation => Ok(self.is_truthy(right)),
      TokenType::Minus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(-n))
        } else {
          Err(format!(""))
        }
      }
      TokenType::Plus => {
        if let Value::Num(n) = right {
          Ok(Value::Num(n.abs()))
        } else {
          Err(format!(""))
        }
      }
      _ => Err(format!("")),
    }
  }
}
