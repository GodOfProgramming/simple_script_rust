use crate::lex::Value;
use std::collections::HashMap;

pub struct Environment {
  values: HashMap<String, Value>,
}

impl Environment {
  pub fn new() -> Environment {
    Environment {
      values: HashMap::new(),
    }
  }

  pub fn define(&mut self, name: &String, value: Value) {
    self.values.insert(name.clone(), value);
  }

  pub fn lookup(&mut self, name: &String) -> Option<&Value> {
    self.values.get(name)
  }
}
