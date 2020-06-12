use crate::complex::{CallErr, NativeFunction};
use crate::env::Env;
use crate::types::Value;
use std::rc::Rc;
use std::time::SystemTime;

pub fn enable(e: &mut Env) {
  e.define(
    String::from("clock"),
    Value::Callee(Rc::new(NativeFunction::new(0, |_| {
      match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => Ok(Value::Num(n.as_nanos() as f64)),
        Err(_) => Err(CallErr {
          msg: String::from(""),
          line: 0, // TODO
        }),
      }
    }))),
  );
}
