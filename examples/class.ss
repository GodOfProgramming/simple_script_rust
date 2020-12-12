class Test {
}

fn new_test() {
  let t = Test();
  t.x = 1;
  t.y = 2;
  return t;
}

let test = new_test();

print test.x;
print test.y;
