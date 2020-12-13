class Test {
  fn test(self, arg) {
    self.x = arg;
  }
}

class Other {
  fn test(this, arg) {
    this.y = arg;
  }
}

fn modify_test(t, arg) {
  t.test(arg);
}

let t1 = Test();
let t2 = Other();

modify_test(t1, 1);
modify_test(t2, 2);

print t1.x;
print t2.y;
