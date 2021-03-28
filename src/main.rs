use simple_script::{Runner, Vm};
use std::{env, fs, path::Path, process};

fn main() {
  let mut exit_code = 0;
  let args: Vec<String> = env::args().collect();
  let vpu = Vm {};
  let runner = Runner::new(vpu);

  if args.len() >= 2 && !run_file(runner, args) {
    exit_code = 1;
  }

  process::exit(exit_code);
}

fn run_file(runner: Runner<Vm>, args: Vec<String>) -> bool {
  let p = Path::new(&args[1]);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => {
        if let Err(errs) = runner.load(&contents) {
          println!("Errors detected when compiling!");
          for err in errs {
            println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
          }
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
