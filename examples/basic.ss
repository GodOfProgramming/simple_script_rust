#!/usr/bin/env ss

fn first_function() {}

let REPS = 100000;
let before = clock_nanos();
for let i = 0; i < REPS; i = i + 1 {
  first_function();
}
let after = clock_nanos();

print "func call time is " + ((after - before) / REPS);

fn second_function(val1, val2) {
  print "val1 is " + val1;
  print "val2 is " + val2;
}

second_function(123, "a value");

fn third_function() {
  "a";
}

print third_function();

fn fourth_function(x) {
  if x * 2 == 10 {
    return "number * 2 is 10";
  } else {
    return "number * 2 is not 10";
  }

  "impossible";
}

print fourth_function(1);
print fourth_function(5);

let foo = 1;
{
  let foo = "some string";
  {
    let foo = true;

    print foo;
  }
  print foo;
}
print foo;

let a = 1;
{
  let a = a + 2;
  print a;
}

let q = true;

if q {
  print "q is true";
} else {
  print "q is false";
}

let one = 1;
let two = 2;

if one == 1 {
  print "1 is 1";
} else if two == 2 {
  print "shouldn't get here, 2 is 2";
} else {
  print "something is wrong";
}

let false_type = false;
let true_type = true;

if true_type or false_type {
  print "passed";
}

if true_type and false_type {
  print "shoudn't pass";
}

print true ? one + one : two + two;

let xyz;

xyz = 1 +
      2 +
      3;

print xyz;

while xyz != 0 {
  print xyz;
  xyz = xyz - 1;
}

for let i = 0; i < 10; i = i + 1 {
  print i;
}
