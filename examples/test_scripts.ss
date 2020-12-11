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
