use simple_script::{Interpreter, VM};
use std::env;
use std::fs;
use std::path::Path;

fn main() {
  let mut exit_code = 0;
  let args: Vec<String> = env::args().collect();
  let mut vm = VM::default();

  if args.contains(&String::from("-h")) {
    if !help(vm) {
      exit_code = 1;
    }
  } else if args.len() >= 2 {
    if !run_file(vm, args) {
      exit_code = 1;
    }
  } else if !vm.cli() {
    exit_code = 1;
  } else {
    eprintln!("usage: ss [path]");
    exit_code = 64;
  }

  std::process::exit(exit_code);
}

fn help(mut vm: VM) -> bool {
  const HELP_SCRIPT: &str = "help.ss";
  const HELP_SCRIPT_SRC: &str = include_str!("help.ss");
  if let Err(err) = vm.run_script(HELP_SCRIPT_SRC) {
    println!("error with help script: {}", err);
    false
  } else {
    true
  }
}

fn run_file(mut vm: VM, args: Vec<String>) -> bool {
  let p = Path::new(&args[1]);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => {
        let file = args[1].clone();
        if let Err(err) = vm.run_script(&contents) {
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
