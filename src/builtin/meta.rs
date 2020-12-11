use crate::ast;
use crate::env::EnvRef;
use crate::lex;
use crate::types::Function;
use crate::types::Value;

pub fn enable(e: &mut EnvRef) {
  e.define(
    String::from("is_defined"),
    Value::Callee(Function::new_native(
      String::from("is_defined"),
      1,
      |env, args| {
        if let Value::Str(name) = &args[0] {
          match env.lookup(name) {
            Some(_) => Ok(Value::Bool(true)),
            None => Ok(Value::Bool(false)),
          }
        } else {
          Err(String::from("value is not a string"))
        }
      },
    )),
  );

  e.define(
    String::from("exec"),
    Value::Callee(Function::new_native(
      String::from("exec"),
      1,
      |env, args| {
        if let Value::Str(script) = &args[0] {
          let analysis = lex::analyze("exec".into(), &script).map_err(|err| format!("{}", err))?;
          let program =
            ast::parse("exec".into(), &analysis.tokens).map_err(|err| format!("{}", err))?;
          ast::exec("exec".into(), env.snapshot(), program).map_err(|err| format!("{}", err))
        } else {
          Err(String::from("value is not string"))
        }
      },
    )),
  );
}
