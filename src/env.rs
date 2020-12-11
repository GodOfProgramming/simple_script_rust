use crate::types::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct Env {
  scope: HashMap<String, Value>,
  enclosing: Option<EnvRef>,
}

impl Default for Env {
  fn default() -> Self {
    Self {
      scope: HashMap::new(),
      enclosing: None,
    }
  }
}

impl Env {
  fn new(enclosing: EnvRef) -> Env {
    Self {
      scope: HashMap::new(),
      enclosing: Some(enclosing),
    }
  }

  fn define(&mut self, name: String, value: Value) {
    self.scope.insert(name, value);
  }

  fn lookup(&self, name: &str) -> Option<Value> {
    if let Some(v) = self.scope.get(name) {
      Some(v.clone())
    } else if let Some(enc) = &self.enclosing {
      enc.env.borrow().lookup(name)
    } else {
      None
    }
  }

  fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
    if self.scope.contains_key(&name) {
      self.scope.insert(name, value);
      Ok(())
    } else if let Some(enc) = &mut self.enclosing {
      enc.env.borrow_mut().assign(name, value)
    } else {
      Err(format!("assignment of undefined variable '{}'", name))
    }
  }
}

#[derive(Clone)]
pub struct EnvRef {
  env: Rc<RefCell<Env>>,
}

impl Default for EnvRef {
  fn default() -> Self {
    Self {
      env: Rc::new(RefCell::new(Env::default())),
    }
  }
}

impl EnvRef {
  pub fn new_with_enclosing(enclosing: EnvRef) -> Self {
    Self {
      env: Rc::new(RefCell::new(Env::new(enclosing))),
    }
  }

  pub fn snapshot(&self) -> Self {
    Self {
      env: Rc::clone(&self.env),
    }
  }

  pub fn define(&mut self, name: String, value: Value) {
    self.env.borrow_mut().define(name, value);
  }

  pub fn lookup(&self, name: &str) -> Option<Value> {
    self.env.borrow().lookup(name)
  }

  pub fn lookup_at(&self, depth: usize, name: &str) -> Option<Value> {
    if let Some(envref) = EnvRef::ancestor(depth, Some(self)) {
      if let Some(v) = envref.env.borrow().scope.get(name) {
        Some(v.clone())
      } else {
        None
      }
    } else {
      None
    }
  }

  pub fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
    self.env.borrow_mut().assign(name, value)
  }

  pub fn assign_at(&mut self, depth: usize, name: String, value: Value) -> Result<(), String> {
    if let Some(envref) = EnvRef::ancestor(depth, Some(self)) {
      envref.env.borrow_mut().assign(name, value)
    } else {
      Err(format!("assignment of undefined variable '{}'", name))
    }
  }

  fn ancestor(depth: usize, enclosing: Option<&EnvRef>) -> Option<EnvRef> {
    if depth == 0 {
      if let Some(enclosing) = enclosing {
        Some(enclosing.snapshot())
      } else {
        None
      }
    } else if let Some(enc) = enclosing {
      if let Some(enc) = &enc.env.borrow().enclosing {
        EnvRef::ancestor(depth - 1, Some(enc))
      } else {
        None
      }
    } else {
      None
    }
  }
}
