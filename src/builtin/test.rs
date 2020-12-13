use crate::env::EnvRef;
use crate::types::{Airity, Value};

pub fn enable(e: &mut EnvRef) {
  e.define_native(String::from("assert"), Airity::Fixed(2), |_env, args| {
    if args[0] != args[1] {
      panic!("test failed, values not equal: {} != {}", args[0], args[1]);
    }

    Ok(Value::Nil)
  });
}
