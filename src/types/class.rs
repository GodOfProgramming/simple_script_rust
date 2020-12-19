use crate::env::EnvRef;
use std::fmt::{self, Display};

#[derive(Clone)]
pub struct Class {
  pub name: String,
  pub static_methods: EnvRef,
  pub instance_methods: EnvRef,
}

impl Class {
  pub fn new(name: String, static_methods: EnvRef, instance_methods: EnvRef) -> Self {
    Self {
      name,
      static_methods,
      instance_methods,
    }
  }
}

impl PartialEq for Class {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)
  }
}
