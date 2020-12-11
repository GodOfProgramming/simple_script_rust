# Load test
{
  # load "examples/test_helpers.ss";
  # assert(load_func(), 1);
}

# Loadr test
{
  # loadr "test_helpers.ss";
  # assert(loadr_func(), 2);
}

# Scope resolution test
# scope 0
assert(1, 1);
let first = 1;
{
  # scope 1
  let a = "global";
  {
    # scope 2
    fn check_a_fn() {
      # scope 3
      assert(a, "global");
    }

    let check_a_closure = || {
      # scope 3
      assert(a, "global");
    };

    # should always pass
    check_a_fn();
    check_a_closure();

    let a = "local";

    # should fail if resolving doesn't function
    check_a_fn();
    check_a_closure();
  }
}
