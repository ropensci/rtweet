test_that("check_expansions works", {
  # Not equal
  expect_error(check_expansions("a", "b"))
  # Two not present
  expect_error(check_expansions(c("a", "c"), "b"))
  # Present and not present
  expect_error(check_expansions(c("a", "b"), "b"))
  # Present
  expect_equal(check_expansions("a", "a"), "a")

  # NULL returns NULL
  expect_equal(check_expansions(NULL, "a"), NULL)
  # Empty vector returns NULL
  expect_equal(check_expansions(c(), "a"), NULL)
  # Empty list returns NULL
  expect_equal(check_expansions(list(), "a"), NULL)
  expect_equal(check_expansions(NA, "a"), NULL)
})

test_that("set_expansions works", {
  expect_error(set_expansions(), NA)
  expect_error(set_expansions(NULL), NA)
  expect_null(set_expansions(NULL, NULL))
  expect_null(set_expansions(NULL, c()))
  expect_error(set_expansions("a"), "These extensions are not allowed: a")
  expect_error(set_expansions(user = "a"), "These extensions are not allowed: a")
})
