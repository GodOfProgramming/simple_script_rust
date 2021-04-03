use super::*;

#[cfg(test)]
fn run<F: FnOnce(Context)>(script: &str, f: F) {
  let vpu = Vpu::default();
  let runner = Runner::new(vpu);
  match runner.load(String::from("test"), script) {
    Ok(mut ctx) => match runner.run(&mut ctx) {
      Ok(_) => f(ctx),
      Err(err) => panic!("{}", err),
    },
    Err(errs) => {
      for err in errs {
        println!("{}", err);
      }
      panic!("compilation errors detected!");
    }
  }
}

#[test]
fn let_0() {
  run("let foo;", |ctx| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::Nil);
  });
}

#[test]
fn let_1() {
  run("let foo = true;", |ctx| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(true));
  });
}

#[test]
fn let_2() {
  run("let foo = 1 + 2 * 3 - 4 / 5 + 6 % 5;", |ctx| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(7.2));
  });
}

#[test]
fn let_3() {
  run("let foo; foo = 1 + 2 * 3 - 4 / 5 + 6 % 5;", |ctx| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(7.2));
  });
}
