use crate::types::{Airity, Class, Function, NativeFn, Value};
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::{self, Display};
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

  fn define(&mut self, name: &str, value: Value) -> bool {
    self.scope.insert(String::from(name), value).is_some()
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

impl PartialEq for Env {
  fn eq(&self, other: &Self) -> bool {
    self.scope == other.scope
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

  pub fn define_native(&mut self, name: &str, airity: Airity, func: NativeFn) -> bool {
    self.define(
      name,
      Value::Callee(Function::new_native(String::from(name), airity, func)),
    )
  }

  pub fn define_class(
    &mut self,
    name: &str,
    static_methods: EnvRef,
    instance_methods: EnvRef,
  ) -> bool {
    self.define(
      name,
      Value::Class(Class {
        name: String::from(name),
        static_methods,
        instance_methods,
      }),
    )
  }

  // returns true if variable was already defined
  pub fn define(&mut self, name: &str, value: Value) -> bool {
    self.env.borrow_mut().define(name, value)
  }

  pub fn lookup(&self, name: &str) -> Option<Value> {
    self.env.borrow().lookup(name)
  }

  pub fn lookup_at(&self, distance: usize, name: &str) -> Option<Value> {
    if let Some(envref) = self.ancestor(distance) {
      if let Some(v) = envref.env.borrow().scope.get(name) {
        Some(v.clone())
      } else {
        None
      }
    } else {
      None
    }
  }

  pub fn get(&self, name: &str) -> Option<Value> {
    if let Some(v) = self.env.borrow().scope.get(name) {
      Some(v.clone())
    } else {
      None
    }
  }

  pub fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
    self.env.borrow_mut().assign(name, value)
  }

  pub fn assign_at(&mut self, depth: usize, name: String, value: Value) -> Result<(), String> {
    if let Some(envref) = self.ancestor(depth) {
      envref.env.borrow_mut().assign(name, value)
    } else {
      Err(format!("assignment of undefined variable '{}'", name))
    }
  }

  fn ancestor(&self, distance: usize) -> Option<EnvRef> {
    let mut env = Rc::clone(&self.env);
    for _ in 0..distance {
      let mut tmp = None;
      if let Some(enc) = &env.borrow().enclosing {
        tmp = Some(Rc::clone(&enc.env));
      }
      env = tmp?;
    }
    Some(EnvRef { env })
  }

  fn fmt_indent(&self, f: &mut fmt::Formatter<'_>, indents: usize) -> fmt::Result {
    let tabs = "\t".repeat(indents);
    writeln!(f, "{}scope: {} {{", tabs, indents)?;
    for (k, v) in self.env.borrow().scope.iter() {
      writeln!(f, "{}{} = {}", "\t".repeat(indents + 1), k, v)?;
    }
    writeln!(f, "{}}}", tabs)?;
    if let Some(enc) = &self.env.borrow().enclosing {
      enc.fmt_indent(f, indents + 1)?;
    }

    Ok(())
  }
}

impl Display for EnvRef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_indent(f, 0)
  }
}

impl PartialEq for EnvRef {
  fn eq(&self, other: &Self) -> bool {
    self.env == other.env
  }
}
