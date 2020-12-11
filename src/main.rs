use simple_script::Interpreter;
use std::env;
use std::fs;
use std::path::Path;

fn main() {
  let mut exit_code = 0;
  let mut inter = Interpreter::default();
  let args: Vec<String> = env::args().collect();

  if args.contains(&String::from("-h")) {
    if !help(inter) {
      exit_code = 1;
    }
  } else if args.len() >= 2 {
    if !run_file(inter, args) {
      exit_code = 1;
    }
  } else if !inter.cli() {
    exit_code = 1;
  }

  std::process::exit(exit_code);
}

fn help(inter: Interpreter) -> bool {
  const HELP_SCRIPT: &str = include_str!("help.ss");
  if let Err(err) = inter.exec("help.ss", HELP_SCRIPT) {
    println!("error with help script: {}", err);
    false
  } else {
    true
  }
}

fn run_file(inter: Interpreter, args: Vec<String>) -> bool {
  let p = Path::new(&args[1]);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => {
        if let Err(err) = inter.exec(&args[1], &contents) {
          println!("{}", err);
          return false;
        }
      }
      Err(err) => {
        println!("{}", err);
        return false;
      }
    }
  } else {
    println!("error:could not find source file '{}'", p.display());
    return false;
  }

  true
}
