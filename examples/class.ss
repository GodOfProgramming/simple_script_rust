class Test {
  fn test(self) {
    let example = "test";
    self.x = "something";
  }
}

let t = Test();
t.test();

print self.x;