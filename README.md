# SimpleScript

## Introduction

First and foremost, this language was not designed to be used standalone, it was meant to be a language that works alongside a Rust program by embedding it. You still can and it'll work, but if you run into problems like "why can't I load a native library", that's why.

It's syntax is similar to Rust in many ways, but naturally has some differences, especially in the safety department. Being a scripting language there are almost no type safety checks.

### Classes

Classes function much like other familiar languages. Declare the class with the `class` keyword and proceed to fill in the body with methods you'd like to associate with it. The notable difference however is there are no special constructor methods. You instantiate a class by calling the class name much like a regular function. However if constructor like methods are desired you can create static methods that are bound to the class. All static methods are prefixed with an '@'. See following example for clarity.

[//]: # "popular languages with known syntax have an advantage here"

```js
class Demo {
```

```rust
  fn() {
    print "This is a constructor";
  }
  
  fn ~() {
    print "This is a destructor";
  }
  
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
