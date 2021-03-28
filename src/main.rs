use simple_script::{Runner, Vm};
use std::{env, fs, path::Path, process};

fn main() {
  let mut exit_code = 0;
  let args: Vec<String> = env::args().collect();
  let vpu = Vm {};
  let runner = Runner::new(vpu);

  if let Some(file) = args.into_iter().nth(1) {
    if !run_file(runner, file) {
      exit_code = 1;
    }
  }

  process::exit(exit_code);
}

fn run_file(runner: Runner<Vm>, file: String) -> bool {
  let p = Path::new(&file);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => {
        if let Err(errs) = runner.load(file, &contents) {
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
