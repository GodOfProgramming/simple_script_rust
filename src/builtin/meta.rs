use crate::ast;
use crate::env::EnvRef;
use crate::lex;
use crate::types::NativeFunction;
use crate::types::Value;
use std::rc::Rc;

pub fn enable(e: EnvRef) {
  e.borrow_mut().define(
    String::from("is_defined"),
    Value::Callee(Rc::new(NativeFunction::new(1, |env, args| {
      if let Value::Str(name) = &args[0] {
        match env.borrow_mut().lookup(name) {
          Some(_) => Ok(Value::Bool(true)),
          None => Ok(Value::Bool(false)),
        }
      } else {
        Err(String::from("value is not a string"))
      }
    }))),
  );

  e.borrow_mut().define(
    String::from("exec"),
    Value::Callee(Rc::new(NativeFunction::new(1, |env, args| {
      if let Value::Str(script) = &args[0] {
        let analysis = lex::analyze("exec", &script).or_else(|err| Err(format!("{}", err)))?;
        let program =
          ast::parse("exec", &analysis.tokens).or_else(|err| Err(format!("{}", err)))?;
        ast::exec("exec", Rc::clone(&env), program).or_else(|err| Err(format!("{}", err)))
      } else {
        Err(String::from("value is not string"))
      }
    }))),
  );
}
