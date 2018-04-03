
context("lookup_collections")

test_that("lookup_collections returns tweets data", {
  skip_on_cran()
  token <- readRDS("twitter_tokens")

  x <- lookup_collections("custom-539487832448843776", token = token)
  class(x)
  expect_equal(is.list(x), TRUE)
  expect_named(x)
  expect_true(all(c("response", "objects") %in% names(x)))
})
