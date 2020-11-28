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
    fn check_a() {
      assert(a, "global");
    }

    # should always pass
    check_a();

    let a = "local";

    # should fail if resolving doesn't function
    check_a();
  }
}
