use crate::env::EnvRef;
use crate::types::NativeFunction;
use crate::types::Value;
use std::rc::Rc;

pub fn enable(e: EnvRef) {
  let dupe = Rc::clone(&e);
  e.borrow_mut().define(
    String::from("is_defined"),
    Value::Callee(Rc::new(NativeFunction::new(1, move |args| {
      if let Value::Str(name) = &args[0] {
        match dupe.borrow_mut().lookup(name) {
          Some(_) => Ok(Value::Bool(true)),
          None => Ok(Value::Bool(false)),
        }
      } else {
        Err(format!("value is not a string"))
      }
    }))),
  );
}
