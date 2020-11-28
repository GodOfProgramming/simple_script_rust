# let a = "global";
# {
#   fn check_a(input) {
#     assert(1, 1)
#   }
#
#   # should always pass
#   check_a(a);
#   let a = "local";
#   # should fail when resolving doesn't function
#   check_a(a);
# }
