test_that("check_expansions works", {
  # Not equal
  expect_error(check_expansions("a", "b"))
  # Two not present
  expect_error(check_expansions(c("a", "c"), "b"))
  # Present and not present
  expect_error(check_expansions(c("a", "b"), "b"))
  # Present
  expect_equal(check_expansions("a", "a"), "a")
  # NULL returns the allowed fields
  expect_equal(check_expansions(NULL, "a"), "a")
})
