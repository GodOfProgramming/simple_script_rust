let REPS = 1;

fn fib(n) {
  if (n < 2) {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

let begin = clock_seconds();
for let i = 0; i < REPS; i = i + 1 {
  fn fib(n) {
    if (n < 2) {
      return n;
    }
    return fib(n - 1) + fib(n - 2);
  }
  print fib(40);
}
let end = clock_seconds();

let time = end - begin;
print "fib 0..20 took " + (time / 20) + " seconds";