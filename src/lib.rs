mod code;
mod types;

#[cfg(test)]
mod test;

use code::{Compiler, Context, OpCode, OpCodeReflection};
use std::fmt::{Debug, Display};
use types::Value;

pub trait New<T> {
  fn new(item: T) -> Self;
}

#[derive(Default, PartialEq)]
pub struct Error {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

impl Error {
  pub fn from_ref(msg: String, opcode_ref: OpCodeReflection) -> Self {
    let mut e = Self {
      msg,
      file: opcode_ref.file,
      line: opcode_ref.line,
      column: opcode_ref.column,
    };
    e.format_with_src_line(opcode_ref.source_line);
    e
  }

  pub fn format_with_src_line(&mut self, src: String) {
    self.msg = format!(
      "{}\n{}\n{}",
      self.msg,
      src,
      format!("{}^", " ".repeat(self.column - 1))
    );
  }
}

impl Debug for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    writeln!(
      f,
      "{} ({}, {}): {}",
      self.file, self.line, self.column, self.msg
    )
  }
}

pub trait Interpreter {
  fn interpret(&self, ctx: &mut Context) -> Result<Value, Error>;
}

#[derive(Default)]
pub struct Vpu;

impl Vpu {
  fn unary_op<F: FnOnce(&mut Context, Value) -> Option<Error>>(
    ctx: &mut Context,
    f: F,
  ) -> Option<Error> {
    if let Some(v) = ctx.stack_pop() {
      f(ctx, v)
    } else {
      Some(ctx.reflect_instruction(|opcode_ref| {
        Error::from_ref(String::from("cannot operate on empty stack"), opcode_ref)
      }))
    }
  }

  fn binary_op<F: FnOnce(&mut Context, Value, Value) -> Option<Error>>(
    ctx: &mut Context,
    f: F,
  ) -> Option<Error> {
    if let Some(bv) = ctx.stack_pop() {
      if let Some(av) = ctx.stack_pop() {
        f(ctx, av, bv)
      } else {
        Some(ctx.reflect_instruction(|opcode_ref| {
          Error::from_ref(String::from("cannot operate on empty stack"), opcode_ref)
        }))
      }
    } else {
      Some(ctx.reflect_instruction(|opcode_ref| {
        Error::from_ref(String::from("cannot operate on empty stack"), opcode_ref)
      }))
    }
  }

  fn global_op<F: FnOnce(&mut Context, String) -> Option<Error>>(
    ctx: &mut Context,
    index: usize,
    f: F,
  ) -> Option<Error> {
    if let Some(name) = ctx.const_at(index) {
      if let Value::Str(name) = name {
        f(ctx, name)
      } else {
        Some(ctx.reflect_instruction(|opcode_ref| {
          Error::from_ref(
            format!("global variable name is not an identifier: {}", name),
            opcode_ref,
          )
        }))
      }
    } else {
      Some(ctx.reflect_instruction(|opcode_ref| {
        Error::from_ref(
          String::from("could not find global variable name, this is most likely a parse error"),
          opcode_ref,
        )
      }))
    }
  }
}

impl Interpreter for Vpu {
  fn interpret(&self, ctx: &mut Context) -> Result<Value, Error> {
    if cfg!(debug_assertions) {
      ctx.display_opcodes();
    }
    while !ctx.done() {
      let opcode = ctx.next();

      if cfg!(debug_assertions) {
        ctx.display_instruction(&opcode, ctx.ip);
      }

      match opcode {
        OpCode::NoOp => break,
        OpCode::Const(index) => {
          if let Some(c) = ctx.const_at(index) {
            ctx.stack_push(c);
          } else {
            todo!();
          }
        }
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
        OpCode::LookupGlobal(index) => {
          if let Some(e) = Vpu::global_op(ctx, index, |ctx, name| match ctx.lookup_global(&name) {
            Some(g) => {
              ctx.stack_push(g);
              None
            }
            None => Some(ctx.reflect_instruction(|opcode_ref| {
              Error::from_ref(
                String::from(
                  "could not find global variable name, this is most likely a parse error",
                ),
                opcode_ref,
              )
            })),
          }) {
            return Err(e);
          }
        }
        OpCode::DefineGlobal(index) => {
          if let Some(e) = Vpu::global_op(ctx, index, |ctx, name| {
            if let Some(v) = ctx.stack_pop() {
              if !ctx.define_global(name, v) {
                todo!();
              }
              None
            } else {
              todo!();
            }
          }) {
            return Err(e);
          }
        }
        OpCode::AssignGlobal(index) => {
          if let Some(e) = Vpu::global_op(ctx, index, |ctx, name| {
            if let Some(v) = ctx.stack_pop() {
              if !ctx.assign_global(name, v) {
                todo!();
              }
              None
            } else {
              todo!();
            }
          }) {
            return Err(e);
          }
        }
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
            Err(e) => Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, opcode_ref))),
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
            Err(e) => Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, opcode_ref))),
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
            Err(e) => Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, opcode_ref))),
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
            Err(e) => Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, opcode_ref))),
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
            Err(e) => Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, opcode_ref))),
          }) {
            return Err(e);
          }
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
            Err(e) => Some(ctx.reflect_instruction(|opcode_ref| Error::from_ref(e, opcode_ref))),
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
            if !v.truthy() {
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
    self.vpu.interpret(ctx)
  }
}
