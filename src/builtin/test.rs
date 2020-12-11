use crate::env::EnvRef;
use crate::types::Function;
use crate::types::Value;

pub fn enable(e: &mut EnvRef) {
  e.define(
    String::from("assert"),
    Value::Callee(Function::new_native(
      String::from("assert"),
      2,
      |_env, args| {
        if args[0] != args[1] {
          panic!("test failed, values not equal: {} != {}", args[0], args[1]);
        }

        Ok(Value::Nil)
      },
    )),
  );
}
