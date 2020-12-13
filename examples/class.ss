let x = "global";
class Test {
  fn test() {
    let x = "local member";
    return || {
      return x;
    };
  }
}

let t = Test();
let func = t.test();
print func();