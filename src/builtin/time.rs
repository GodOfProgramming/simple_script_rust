use crate::env::EnvRef;
use crate::types::NativeFunction;
use crate::types::Value;
use std::rc::Rc;
use std::time::SystemTime;

const NANOS_IN_SECOND: f64 = 1_000_000_000.0;

pub fn enable(e: EnvRef) {
  e.borrow_mut().define(
    String::from("clock_nanos"),
    Value::Callee(Rc::new(NativeFunction::new(0, |_| match SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
    {
      Ok(n) => Ok(Value::Num(n.as_nanos() as f64)),
      Err(err) => Err(format!("error querying system time: {}", err)),
    }))),
  );

  e.borrow_mut().define(
    String::from("clock_seconds"),
    Value::Callee(Rc::new(NativeFunction::new(0, |_| match SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
    {
      Ok(n) => Ok(Value::Num(n.as_nanos() as f64 / NANOS_IN_SECOND)),
      Err(err) => Err(format!("error querying system time: {}", err)),
    }))),
  );
}
