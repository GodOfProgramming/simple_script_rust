use crate::env::EnvRef;
use std::fmt::{self, Display};

#[derive(Clone)]
pub struct Instance {
  pub instance_of: String,
  pub methods: EnvRef,
  pub members: EnvRef,
}

impl PartialEq for Instance {
  fn eq(&self, other: &Self) -> bool {
    self.instance_of == other.instance_of && self.members == other.members
  }
}

impl Display for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.instance_of)
  }
}
