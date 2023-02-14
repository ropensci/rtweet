test_that("multiplication works", {
  expect_null(arg_def(NULL, "b"))
  expect_equal(arg_def(NA, "b"), "b")
  expect_equal(arg_def(NA_character_, "b"), "b")
  expect_equal(arg_def(c("a", "b"), "b"), c("a", "b"))
  arg <- list("a" = letters[1:2], "b" = letters[2:3])
  expect_equal(arg_def(arg, "b"), arg)
})
