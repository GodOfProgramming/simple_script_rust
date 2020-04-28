use simple_script::core::Interpreter;
use std::env;
use std::io;
use std::fs;
use std::path::Path;

fn help() -> String {
  format!("{}\n", "Usage: iss [optional script]")
}

fn main() -> io::Result<()> {
  let mut i = Interpreter::new();

  let args: Vec<String> = env::args().collect();

  if args.len() > 2 {
    println!("{}", help());
  } else if args.len() == 2 {
    let p = Path::new(&args[1]);
    if p.exists() {
      let contents = fs::read_to_string(p)?;
      i.run_script(contents);
    }
  } else {
    i.interactive_mode()?;
  }

  Ok(())
}
