use crate::lex::Value;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub struct Env {
  values: HashMap<String, Value>,
}

pub type EnvRef = Rc<RefCell<Env>>;

impl Env {
  pub fn new() -> Env {
    Env {
      values: HashMap::new(),
    }
  }

  pub fn define(&mut self, name: String, value: Value) {
    self.values.insert(name, value);
  }

  pub fn lookup(&self, name: &String) -> Option<&Value> {
    self.values.get(name)
  }

  pub fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
    if self.values.contains_key(&name) {
      self.values.insert(name, value);
      Ok(())
    } else {
      Err(format!("assignment of undefined variable '{}'", name))
    }
  }
}
