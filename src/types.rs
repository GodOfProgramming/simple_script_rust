mod class;
mod function;
mod instance;
mod value;


pub trait Visitor<T, R> {
  fn visit(&mut self, _: &T) -> R;
}
