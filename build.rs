use std::fs::{self, ReadDir};
use std::process::Command;

fn main() {
  if !cfg!(windows) {
    Command::new("ruby")
      .arg("tools/ast_gen.rb")
      .status()
      .unwrap();

    rerun_dir("tools");
    rerun_dir("config");
  }
}

fn rerun_dir(dir: &str) {
  let paths: ReadDir = fs::read_dir(dir).unwrap();

  for path in paths {
    rerun_file(&path.unwrap().path().display().to_string());
  }
}

fn rerun_file(file: &str) {
  println!("cargo:rerun-if-changed={}", file)
}
