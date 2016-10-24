context("utils")

test_that("utils", {
  skip_on_cran()

  expect_true("mpg" %xy% mtcars)
  expect_true(!"year" %xy% mtcars)
  expect_true(identical(length(c("mpg", "year") %xy% mtcars), 2L))
  expect_true(c("mpg", "year") %any% mtcars)
  expect_true(!c("mpg", "year") %all% mtcars)
})
