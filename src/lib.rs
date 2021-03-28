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

#[derive(Debug, PartialEq)]
pub struct Error {
  pub msg: String,
  pub file: String,
  pub line: usize,
  pub column: usize,
}

pub trait Vpu {
  fn process(&self, ctx: &mut Context);
}

pub struct Vm;

impl Vm {
  fn unary_op<F: FnOnce(&mut Context, Value)>(ctx: &mut Context, f: F) {
    match ctx.stack_pop() {
      Some(v) => f(ctx, v),
      None => todo!(),
    }
  }

  fn binary_op<F: FnOnce(&mut Context, Value, Value)>(ctx: &mut Context, f: F) {
    match ctx.stack_pop() {
      Some(av) => match ctx.stack_pop() {
        Some(bv) => f(ctx, av, bv),
        None => todo!(),
      },
      None => todo!(),
    };
  }
}

impl Vpu for Vm {
  fn process(&self, ctx: &mut Context) {
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
        OpCode::Equal => Vm::binary_op(ctx, |ctx, a, b| ctx.stack_push(Value::new(a == b))),
        OpCode::NotEqual => Vm::binary_op(ctx, |ctx, a, b| ctx.stack_push(Value::new(a != b))),
        OpCode::Greater => Vm::binary_op(ctx, |ctx, a, b| ctx.stack_push(Value::new(a > b))),
        OpCode::GreaterEqual => Vm::binary_op(ctx, |ctx, a, b| ctx.stack_push(Value::new(a >= b))),
        OpCode::Less => Vm::binary_op(ctx, |ctx, a, b| ctx.stack_push(Value::new(a < b))),
        OpCode::LessEqual => Vm::binary_op(ctx, |ctx, a, b| ctx.stack_push(Value::new(a <= b))),
        OpCode::Check => match ctx.stack_pop() {
          Some(a) => match ctx.stack_peek() {
            Some(b) => ctx.stack_push(Value::new(a == b)),
            None => todo!(),
          },
          None => todo!(),
        },
        OpCode::Add => Vm::binary_op(ctx, |ctx, a, b| match a + b {
          Ok(v) => ctx.stack_push(v),
          Err(e) => todo!("TODO: {}", e),
        }),
        OpCode::Sub => Vm::binary_op(ctx, |ctx, a, b| match a - b {
          Ok(v) => ctx.stack_push(v),
          Err(e) => todo!("TODO: {}", e),
        }),
        OpCode::Mul => Vm::binary_op(ctx, |ctx, a, b| match a * b {
          Ok(v) => ctx.stack_push(v),
          Err(e) => todo!("TODO: {}", e),
        }),
        OpCode::Div => Vm::binary_op(ctx, |ctx, a, b| match a / b {
          Ok(v) => ctx.stack_push(v),
          Err(e) => todo!("TODO: {}", e),
        }),
        OpCode::Mod => Vm::binary_op(ctx, |ctx, a, b| match a % b {
          Ok(v) => ctx.stack_push(v),
          Err(e) => todo!("TODO: {}", e),
        }),
        OpCode::Not => Vm::unary_op(ctx, |ctx, v| ctx.stack_push(!v)),
        OpCode::Negate => Vm::unary_op(ctx, |ctx, v| match -v {
          Ok(n) => ctx.stack_push(n),
          Err(e) => todo!("TODO: {}", e),
        }),
        OpCode::Print => Vm::unary_op(ctx, |_, v| println!("{}", v)),
        OpCode::Swap => Vm::binary_op(ctx, |ctx, a, b| {
          ctx.stack_push(a);
          ctx.stack_push(b);
        }),
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
          // TODO check if this should really be peek
          Some(v) => {
            if v.truthy() {
              ctx.jump(count);
              continue;
            }
          }
          None => todo!(),
        },
        OpCode::And(count) => match ctx.stack_peek() {
          // TODO check if this should really be peek
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
  }
}

pub struct Runner<T: Vpu> {
  vpu: T,
}

impl<T: Vpu> Runner<T> {
  pub fn new(vpu: T) -> Self {
    Runner { vpu }
  }

  pub fn load(&self, file: String, code: &str) -> Result<Context, Vec<Error>> {
    let compiler = Compiler {};

    compiler.compile(&file, code)?;

    let instructions = Vec::new();
    let meta = CodeMeta {};
    let ctx = Context::new(instructions, meta);
    Ok(ctx)
  }

  pub fn run(&self, ctx: &mut Context) -> Result<Value, Error> {
    unimplemented!("vpu process here");
  }
}
