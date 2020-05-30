use simple_script::core::Interpreter;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

fn main() -> Result<(), String> {
  let inter = Interpreter::new();
  let args: Vec<String> = env::args().collect();

  if args.contains(&String::from("-h")) {
    help(inter)?;
  } else if args.len() >= 2 {
    run_file(inter, args)?;
  } else {
    run_interactive(inter)?;
  }

  Ok(())
}

fn help(inter: Interpreter) -> Result<(), String> {
  const HELP_SCRIPT: &str =
  r#"
  print "Simple Script Interpreter";
  print "";
  print "Usage: ss [optional_script]";
  print "";
  print "Running without the script will start interactive mode.";
  print "Here you can execute a series of statements line by line for real time feedback.";
  print "";
  "#;
  if let Err(err) = inter.exec(HELP_SCRIPT) {
    return Err(format!("this shouldn't happen, please report it: {}", err.msg));
  } else {
    Ok(())
  }
}

fn run_file(inter: Interpreter, args: Vec<String>) -> Result<(), String> {
    let p = Path::new(&args[1]);
    if p.exists() {
      match fs::read_to_string(p) {
        Ok(contents) => {
          if let Err(err) = inter.exec(&contents) {
            println!("Fatal: could not run script: {}: line {}", err.msg, err.line + 1);
            return Err(err.msg);
          }
        }
        Err(err) => {
          return Err(format!("{}", err));
        }
      }
    } else {
      println!("Fatal: could not find source file '{}'", p.display());
    }

    Ok(())
}

fn run_interactive(inter: Interpreter) -> Result<(), String> {
    let mut input = String::new();
    let exit = false;
    let mut line_number = 1;

    while !exit {
      print!("ss(main):{}> ", line_number);
      if let Err(err) = io::stdout().flush() {
        return Err(format!("{}", err));
      }

      if let Err(err) = io::stdin().read_line(&mut input) {
        return Err(format!("{}", err));
      }

      match inter.exec(&input) {
        Ok(res) => {
          println!("=> {}", res.value);
          line_number += res.lines;
        }
        Err(err) => println!("{}: line {}", err.msg, line_number + err.line),
      }

      input.clear();
    }

    Ok(())
}
