run_test("resolving", |t| {
  t.exec("it resolves correctly", |t| {
    var a = "global";
    {
      fn check_a(input) {
        t.eq(a, input)
      }

      // should always pass
      check_a(a);
      var a = "local";
      // should fail when resolving doesn't function
      check_a(a);
    }
  });
});