# Load test
{
  load "examples/test_helpers.ss";
  assert(load_func(), 1);
}

# Loadr test
{
  loadr "test_helpers.ss";
  assert(loadr_func(), 2);
}

# Scope resolution test
{
  let a = "global";
  {
    fn check_a_fn() {
      assert(a, "global");
    }

    let check_a_closure = || {
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
