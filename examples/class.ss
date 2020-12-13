class Test {
  fn test(self) {
    let example = "test";
    self.x = "something";
  }
}

let t = Test();
print_env(t);
t.test();

print self.x;