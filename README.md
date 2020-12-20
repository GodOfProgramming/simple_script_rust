# SimpleScript

## Introduction

First and foremost, this language was not designed to be used standalone, it was meant to be a language that works alongside a Rust program by embedding it. You still can and it'll work, but if you run into problems like "why can't I load a native library", that's why.

It's syntax is similar to Rust in many ways, but naturally has some differences, especially in the safety department. Being a scripting language there are almost no type safety checks, and how their handled is much different than most other languages.

## Features

### ! Unimplemented: Dedicated Error Type

Unlike other scripting languages where dividing a string by 0 will result in a runtime error, SimpleScript handles it a bit differently. What it does is in that specific instance, will return an error type. In every language, at the end of the day there is an underlying data type for every variable at one point or another. Some will not let you inspect that though, instead they will trust you to know what you're doing, and cause a runtime error if that is not the case.

Introduced here is the combination of error reporting and type checking. While this is a dynamic language, you can type check at runtime with the `is` keyword. The is keyword will return true if a type matches the specified type on the right hand side. See example below.

```rust
let a = 1;
let b = 0;

let result = a / b;

if result is error {
  print result;
  exit(1);
} else {
  perform_operation_on_number(result)
}

```

You can also create your own errors using the `error` function. Errors can hold any type in their "value" field. By default that is what is printed when you print an error, or try to concatenate it into a bigger string.

```rust
fn div(a, b) {
  if b == 0 {
    return error("cannot divide by 0");
  } else {
    return a / b;
  }
}

let x = 1;
let y = 0;

let result = div(x, y);

if result is error {
  exit(1);
} else {
  print result;
}

```

### Classes

Classes function much like other familiar languages. Declare the class with the `class` keyword and proceed to fill in the body with methods you'd like to associate with it. The notable difference however is there are no special constructor methods. You instantiate a class by calling the class name much like a regular function. However if constructor like methods are desired you can create static methods that are bound to the class. All static methods are prefixed with an '@'. See following example for clarity.

[//]: # "popular languages with known syntax have an advantage here"

```js
class Demo {
```

```rust
  fn @static_method(param1, param2) {
    let instance = Demo();
    instance.x = param1;
    instance.y = param2;
    return instance;
  }



  fn instance_method(self, param1, param2) {
    self.x = param1;
    self.y = param2;
  }
```

```js
}


let demo = Demo.static_method("x's value", "y's value");
```

```perl
print demo;
```

### Lambdas

They behave the same as any other language, they're pretty much syntactic sugar for passing functions around. Example of equivalent operations below.

```rust
fn do_func(f) {
  f();
}

fn function() {
  print "function";
}

let f = function;

let lambda = || {
  print "lambda"
};

do_func(function);
do_func(f);
do_func(lambda);

```

However the biggest difference is lambdas can self execute, and functions cannot. That is because lambdas are evaluated as expressions and functions are declarative statements.

```rust
|| {
  while true {
    print "never stop";
  }
}();
```
