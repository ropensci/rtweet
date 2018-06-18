context("bearer_token")

test_that("bearer_token functions", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- search_tweets("lang:en", n = 300, token = bearer_token(token))
  expect_true(is.data.frame(x))
  expect_gt(nrow(x), 50)
})
