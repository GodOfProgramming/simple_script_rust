use crate::ast;
use crate::env::EnvRef;
use crate::lex;
use crate::types::{Airity, Value};

pub fn enable(e: &mut EnvRef) {
  e.define_native(
    String::from("print_env"),
    Airity::Range(0..=1),
    |env, args| {
      if args.is_empty() {
        println!("<current env>\n{}", env);
      } else {
        match &args[0] {
          Value::Instance {
            instance_of,
            methods,
            members,
          } => {
            println!(
              "<instance of {}>\n<methods>\n{}<members>\n{}",
              instance_of, methods, members
            );
          }
          Value::Class { name, methods } => {
            println!("<class {}>\n<methods>\n{}", name, methods);
          }
          x => return Err(format!("cannot print env for type {}", x)),
        }
      }
      Ok(Value::Nil)
    },
  );

  e.define_native(String::from("is_defined"), Airity::Fixed(1), |env, args| {
    if let Value::Str(name) = &args[0] {
      match env.lookup(name) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
      }
    } else {
      Err(String::from("value is not a string"))
    }
  });

  e.define_native(String::from("exec"), Airity::Fixed(1), |env, args| {
    if let Value::Str(script) = &args[0] {
      let analysis = lex::analyze("exec".into(), &script).map_err(|err| format!("{}", err))?;
      let program =
        ast::parse("exec".into(), &analysis.tokens).map_err(|err| format!("{}", err))?;
      ast::exec("exec".into(), env.snapshot(), program).map_err(|err| format!("{}", err))
    } else {
      Err(String::from("value is not string"))
    }
  });
}
