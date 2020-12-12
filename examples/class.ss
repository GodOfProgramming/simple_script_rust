let x = 100;

fn foo() {
  print "fn " + x;
}

class Test {
  fn foo() {
    print "class " + x;
  }
}

{
  let test = Test();

  let x = 200;
  test.foo();
  foo();
}
