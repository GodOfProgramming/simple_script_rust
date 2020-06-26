fn fib(n) {
  if n <= 1 {
    n;
  } else {
    fib(n - 2) + fib(n - 1);
  }
}

fn new_fib() {
  var c = |n| {
    if n <= 1 {
      n;
    } else {
      c(n - 2) + c(n - 1);
    }
  };

  c;
}

var begin = clock_seconds();
for var i = 0; i < 20; i = i + 1 {
  var f = new_fib();
  print f(i);
}
var end = clock_seconds();

var new_fib_time = end - begin;
print "new_fib 0..20 took " + new_fib_time + " seconds";

begin = clock_seconds();
for var i = 0; i < 20; i = i + 1 {
  print fib(i);
}
end = clock_seconds();

var fib_time = end - begin;
print "fib 0..20 took " + fib_time + " seconds";

if fib_time > new_fib_time {
  print "closure is faster than functions";
} else if fib_time < new_fib_time {
  print "functions are faster than closures";
} else {
  print "they performed the same";
}
