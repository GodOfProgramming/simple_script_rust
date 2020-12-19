use crate::env::EnvRef;
use std::fmt::{self, Display};

#[derive(Clone)]
pub struct Class {
  pub name: String,
  pub methods: EnvRef,
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
