use crate::env::EnvRef;
use crate::types::Function;
use crate::types::Value;
use std::process;

pub fn enable(e: &mut EnvRef) {
  e.define(
    String::from("exit"),
    Value::Callee(Function::new_native(
      String::from("exit"),
      1,
      |_env, args| {
        let code;
        if let Value::Num(c) = args[0] {
          code = c as i32;
        } else {
          return Err(format!("unable to convert {} to integer", args[0]));
        }
        process::exit(code);
      },
    )),
  );
}
