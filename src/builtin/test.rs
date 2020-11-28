use crate::env::EnvRef;
use crate::types::NativeFunction;
use crate::types::Value;
use std::rc::Rc;

pub fn enable(e: EnvRef) {
  e.borrow_mut().define(
    String::from("assert"),
    Value::Callee(Rc::new(NativeFunction::new(2, |_env, args| {
      if args[0] != args[1] {
        panic!("test failed, values not equal: {} != {}", args[0], args[1]);
      }

      Ok(Value::Nil)
    }))),
  );
}
