use crate::env::EnvRef;
use crate::types::{Airity, New, Value};
use std::time::SystemTime;

const NANOS_IN_SECOND: f64 = 1_000_000_000.0;

pub fn enable(e: &mut EnvRef) {
  e.define_native(
    "clock_nanos",
    Airity::Fixed(0),
    |_, _| match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
      Ok(n) => Ok(Value::new(n.as_nanos() as f64)),
      Err(err) => Err(format!("error querying system time: {}", err)),
    },
  );

  e.define_native(
    "clock_seconds",
    Airity::Fixed(0),
    |_, _| match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
      Ok(n) => Ok(Value::Num(n.as_nanos() as f64 / NANOS_IN_SECOND)),
      Err(err) => Err(format!("error querying system time: {}", err)),
    },
  );
}
