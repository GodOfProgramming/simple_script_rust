let x = 100;

class Test {
  fn foo() {
    print x;
  }
}

fn foo() {
  print x;
}

let test = Test();

{
  let x = 200;
  test.foo();
  foo();
}
