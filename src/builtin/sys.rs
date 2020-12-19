use crate::env::EnvRef;
use crate::types::{Airity, Value};
use std::process;

pub fn enable(e: &mut EnvRef) {
  e.define_native("exit", Airity::Fixed(1), |_env, args| {
    let code = if let Value::Num(c) = args[0] {
      c as i32
    } else {
      return Err(format!("unable to convert {} to integer", args[0]));
    };
    process::exit(code);
  });
}
