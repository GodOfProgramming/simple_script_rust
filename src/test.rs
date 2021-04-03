use super::*;

#[cfg(test)]
fn run<F: FnOnce(Context, Value)>(script: &str, f: F) {
  let vpu = Vpu::default();
  let runner = Runner::new(vpu);
  match runner.load(String::from("test"), script) {
    Ok(mut ctx) => match runner.run(&mut ctx) {
      Ok(v) => f(ctx, v),
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

/**
 * nil
 */
#[test]
fn let_0() {
  run("let foo;", |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::Nil);
  });
}

/**
 * true
 */
#[test]
fn let_1() {
  run("let foo = true;", |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(true));
  });
}

/**
 * add sub mul div mod
 */
#[test]
fn let_2() {
  run("let foo = 1 + 2 * 3 - 4 / 5 + 6 % 5;", |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(7.2));
  });
}

/**
 * add sub mul div mod
 */
#[test]
fn let_3() {
  run("let foo; foo = 1 + 2 * 3 - 4 / 5 + 6 % 5;", |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(7.2));
  });
}

#[test]
fn block_0() {
  run("let foo; { foo = 1; }", |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(1));
  });
}

#[test]
fn block_1() {
  run("let foo; { foo = 1; let bar; bar = 0; }", |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(1));
    assert!(ctx.lookup_global("bar").is_none());
  });
}

#[test]
fn end_0() {
  run(
    "let foo; { foo = 1; let bar; bar = 0; { end bar; } }",
    |ctx, v| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(1));
      assert!(ctx.lookup_global("bar").is_none());
      assert_eq!(v, Value::new(0));
    },
  );
}

#[test]
fn if_0() {
  const SCRIPT: &str = "let foo = true; if foo { foo = 1; }";
  run(SCRIPT, |ctx, _| {
    let val = ctx.lookup_global("foo").unwrap();
    assert_eq!(val, Value::new(1));
  });
}

/**
 * else block not executed
 */
#[test]
fn if_1() {
  run(
    "let foo = true; if foo { foo = 1; } else { foo = 2; }",
    |ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(1));
    },
  );
}

#[test]
fn if_2() {
  run(
    "let foo = false; if foo { foo = 1; } else { foo = 2; }",
    |ctx, _| {
      let val = ctx.lookup_global("foo").unwrap();
      assert_eq!(val, Value::new(2));
    },
  );
}
