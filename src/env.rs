use crate::lex::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Env {
  values: HashMap<String, Value>,
  enclosing: Option<EnvRef>,
}

pub type EnvRef = Rc<RefCell<Env>>;

impl Env {
  pub fn new() -> Self {
    Self {
      values: HashMap::new(),
      enclosing: None,
    }
  }

  pub fn new_with_enclosing(enclosing: EnvRef) -> Self {
    Self {
      values: HashMap::new(),
      enclosing: Some(enclosing),
    }
  }

  pub fn define(&mut self, name: String, value: Value) {
    self.values.insert(name, value);
  }

  pub fn lookup(&self, name: &String) -> Option<Value> {
    if let Some(v) = self.values.get(name) {
      Some(v.clone())
    } else if let Some(e) = &self.enclosing {
      e.borrow().lookup(name)
    } else {
      None
    }
  }

  pub fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
    if self.values.contains_key(&name) {
      self.values.insert(name, value);
      Ok(())
    } else if let Some(e) = &mut self.enclosing {
      e.borrow_mut().assign(name, value)
    } else {
      Err(format!("assignment of undefined variable '{}'", name))
    }
  }
}
