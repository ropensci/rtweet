context("mentions")

test_that("mentions returns tweets data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- get_mentions(token = token)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 2)
  expect_gt(ncol(x), 2)
})
