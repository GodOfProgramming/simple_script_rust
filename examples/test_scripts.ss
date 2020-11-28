let a = "global";
{
  fn check_a() {
    assert(a, "global");
  }

  # should always pass
  check_a();

  let a = "local";

  # should fail when resolving doesn't function
  check_a();
}
