class Test {
  fn @test() {
    print "static";
  }

  fn test(self) {
    print "instance";
  }
}

fn test() {
  print "fn";
}

print_env(Test);

test();
Test.test();
Test().test();
