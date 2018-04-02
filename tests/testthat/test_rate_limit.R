context("rate_limit")

test_that("rate_limit returns rate_limit data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- rate_limit(list(token, token))

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("remaining" %in% names(x))
  expect_gt(nrow(x), 2)
  expect_gt(ncol(x), 2)
})

