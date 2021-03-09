test_that("bearer_token functions", {
  x <- search_tweets("lang:en", n = 300, token = bearer_token())
  expect_true(is.data.frame(x))
  expect_gt(nrow(x), 50)
})
