use simple_script::core::Interpreter;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

fn help() -> String {
  format!("{}\n", "Usage: iss [optional script]")
}

fn main() -> Result<(), String> {
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
          if let Err(err) = inter.exec(&contents) {
            println!("Fatal: could not run script: {}: {}", err.msg, err.line + 1);
          }
        }
        Err(err) => {
          return Err(format!("{}", err));
        }
      }
    } else {
      println!("Fatal: could not find source file '{}'", p.display());
    }
  } else {
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
        Err(err) => println!("{}: {}", err.msg, line_number + err.line),
      }

      input.clear();
    }
  }

  Ok(())
}
