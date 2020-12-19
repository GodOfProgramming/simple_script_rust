mod class;
mod function;
mod instance;
mod value;

pub use class::Class;
pub use function::{Airity, Function, NativeFn};
pub use instance::Instance;
pub use value::{Value, Values};

pub trait Visitor<T, R> {
  fn visit(&mut self, _: &T) -> R;
}

pub trait ValueError<T> {
  fn new_err(err: T) -> Self;
}

pub trait New<T> {
  fn new(item: T) -> Self;
}
