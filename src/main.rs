use simple_script::core::Interpreter;
use std::env;
use std::fs;
use std::io;
use std::path::Display;
use std::path::Path;

fn help() -> String {
  format!("{}\n", "Usage: iss [optional script]")
}

fn main() -> Result<(), ()> {
  let inter = Interpreter::new();
  let args: Vec<String> = env::args().collect();

  if args.contains(&String::from("-h")) {
    println!("{}", help());
  } else if args.len() == 2 {
    // from file
    let p = Path::new(&args[1]);
    if p.exists() {
      match fs::read_to_string(p) {
        Ok(contents) => {
          if let Err(line) = inter.exec(&contents) {
            println!("Fatal: could not run script, error on line {}", line);
          }
        }
        Err(_) => {
          return Err(());
        }
      }
    } else {
      println!("Fatal: could not find source file '{}'", p.display());
    }
  } else {
    if let Err(err) = inter.run_interactive() {
      println!("Fatal: could not start interactive mode: {}", err);
    }
  }

  Ok(())
}
