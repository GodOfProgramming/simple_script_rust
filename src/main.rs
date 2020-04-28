use simple_script::core::Interpreter;
use std::io;

fn main() -> io::Result<()> {
  let mut interpreter = Interpreter::new();

  while interpreter.is_active() {
    interpreter.read_from_stdin()?;
  }

  Ok(())
}
