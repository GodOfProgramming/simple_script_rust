mod code;
mod types;

use code::CodeMeta;
use code::Compiler;
use code::Context;
use code::OpCode;
use types::Value;

pub trait New<T> {
  fn new(item: T) -> Self;
}

#[derive(Default, Debug, PartialEq)]
pub struct Error {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl Error {
  pub fn format_with_src_line(&mut self, src: String) {
    self.msg = format!(
      "{}\n{}\n{}",
      self.msg,
      src,
      format!("{}^", " ".repeat(self.column - 1))
    );
  }
}

pub trait Interpreter {
  fn process(&self, ctx: &mut Context) -> Result<Value, Error>;
}

pub struct Vpu;

impl Vpu {
  fn unary_op<F: FnOnce(&mut Context, Value) -> Option<Error>>(
    ctx: &mut Context,
    f: F,
  ) -> Option<Error> {
    match ctx.stack_pop() {
      Some(v) => f(ctx, v),
      None => Some(ctx.reflect_instruction(|src, file, line, column| {
        let mut e = Error {
          msg: String::from("cannot operate on empty stack"),
          file,
          line,
          column,
        };
        e.format_with_src_line(src);
        e
      })),
    }
  }

  fn binary_op<F: FnOnce(&mut Context, Value, Value) -> Option<Error>>(
    ctx: &mut Context,
    f: F,
  ) -> Option<Error> {
    match ctx.stack_pop() {
      Some(av) => match ctx.stack_pop() {
        Some(bv) => f(ctx, av, bv),
        None => Some(ctx.reflect_instruction(|src, file, line, column| {
          let mut e = Error {
            msg: String::from("cannot operate on empty stack"),
            file,
            line,
            column,
          };
          e.format_with_src_line(src);
          e
        })),
      },
      None => Some(ctx.reflect_instruction(|src, file, line, column| {
        let mut e = Error {
          msg: String::from("cannot operate on empty stack"),
          file,
          line,
          column,
        };
        e.format_with_src_line(src);
        e
      })),
    }
  }
}

impl Interpreter for Vpu {
  fn process(&self, ctx: &mut Context) -> Result<Value, Error> {
    while !ctx.done() {
      match ctx.next() {
        OpCode::NoOp => break,
        OpCode::Const(index) => match ctx.const_at(index) {
          Some(c) => ctx.stack_push(c),
          None => todo!(),
        },
        OpCode::Nil => ctx.stack_push(Value::Nil),
        OpCode::True => ctx.stack_push(Value::new(true)),
        OpCode::False => ctx.stack_push(Value::new(false)),
        OpCode::Pop => {
          ctx.stack_pop();
        }
        OpCode::PopN(count) => ctx.stack_pop_n(count),
        OpCode::LookupLocal(index) => match ctx.stack_index(index) {
          Some(l) => ctx.stack_push(l),
          None => todo!(),
        },
        OpCode::AssignLocal(index) => match ctx.stack_peek() {
          Some(v) => ctx.stack_assign(index, v),
          None => todo!(),
        },
        OpCode::LookupGlobal(name) => match ctx.lookup_global(&name) {
          Some(g) => ctx.stack_push(g),
          None => todo!(),
        },
        OpCode::DefineGlobal(name) => match ctx.stack_pop() {
          Some(v) => {
            if !ctx.define_global(name, v) {
              todo!();
            }
          }
          None => todo!(),
        },
        OpCode::AssignGlobal(name) => match ctx.stack_pop() {
          Some(v) => {
            if !ctx.assign_global(name, v) {
              todo!();
            }
          }
          None => todo!(),
        },
        OpCode::Equal => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(Value::new(a == b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::NotEqual => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(Value::new(a != b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Greater => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(Value::new(a > b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::GreaterEqual => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(Value::new(a >= b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Less => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(Value::new(a < b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::LessEqual => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(Value::new(a <= b));
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Check => match ctx.stack_pop() {
          Some(a) => match ctx.stack_peek() {
            Some(b) => ctx.stack_push(Value::new(a == b)),
            None => todo!(),
          },
          None => todo!(),
        },
        OpCode::Add => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| match a + b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => Some(ctx.reflect_instruction(|file, src, line, column| {
              let mut e = Error {
                msg: e,
                file,
                line,
                column,
              };
              e.format_with_src_line(src);
              e
            })),
          }) {
            return Err(e);
          }
        }
        OpCode::Sub => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| match a - b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => todo!("TODO: {}", e),
          }) {
            return Err(e);
          }
        }
        OpCode::Mul => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| match a * b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => todo!("TODO: {}", e),
          }) {
            return Err(e);
          }
        }
        OpCode::Div => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| match a / b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => todo!("TODO: {}", e),
          }) {
            return Err(e);
          }
        }
        OpCode::Mod => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| match a % b {
            Ok(v) => {
              ctx.stack_push(v);
              None
            }
            Err(e) => todo!("TODO: {}", e),
          }) {
            return Err(e);
          }
        }
        OpCode::Not => {
          if let Some(e) = Vpu::unary_op(ctx, |ctx, v| {
            ctx.stack_push(!v);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Negate => {
          if let Some(e) = Vpu::unary_op(ctx, |ctx, v| match -v {
            Ok(n) => {
              ctx.stack_push(n);
              None
            }
            Err(e) => Some(ctx.reflect_instruction(|file, src, line, column| {
              let mut e = Error {
                msg: e,
                file,
                line,
                column,
              };
              e.format_with_src_line(src);
              e
            })),
          }) {
            return Err(e);
          }
        }
        OpCode::Print => {
          if let Some(e) = Vpu::unary_op(ctx, |_, v| {
            println!("{}", v);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Swap => {
          if let Some(e) = Vpu::binary_op(ctx, |ctx, a, b| {
            ctx.stack_push(a);
            ctx.stack_push(b);
            None
          }) {
            return Err(e);
          }
        }
        OpCode::Jump(count) => ctx.jump(count),
        OpCode::JumpIfFalse(count) => match ctx.stack_peek() {
          Some(v) => {
            if v.truthy() {
              ctx.jump(count);
              continue; // ip is now at correct place, so skip advance
            }
          }
          None => todo!(),
        },
        OpCode::Loop(count) => {
          ctx.loop_back(count);
          continue; // ip is at correct place
        }
        OpCode::Or(count) => match ctx.stack_peek() {
          // TODO check if this should really be peek, think so because a pop happens after(?)
          Some(v) => {
            if v.truthy() {
              ctx.jump(count);
              continue;
            }
          }
          None => todo!(),
        },
        OpCode::And(count) => match ctx.stack_peek() {
          // TODO check if this should really be peek, think so because a pop happens after(?)
          Some(v) => {
            if !v.truthy() {
              ctx.jump(count);
              continue;
            }
          }
          None => todo!(),
        },
        x => unimplemented!("Unimplemented: {:?}", x),
      }
      ctx.advance();
    }

    Ok(Value::Nil)
  }
}

pub struct Runner<T: Interpreter> {
  vpu: T,
}

impl<T: Interpreter> Runner<T> {
  pub fn new(vpu: T) -> Self {
    Runner { vpu }
  }

  pub fn load(&self, file: String, code: &str) -> Result<Context, Vec<Error>> {
    let compiler = Compiler {};
    compiler.compile(&file, code)
  }

  pub fn run(&self, ctx: &mut Context) -> Result<Value, Error> {
    self.vpu.process(ctx)
  }
}
