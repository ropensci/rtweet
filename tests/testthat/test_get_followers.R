context("get_followers")

test_that("get_followers returns data frame with user_id", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  f <- get_followers("HillaryClinton", n = 10000, token = token)

  expect_true(is.data.frame(f))
  expect_true(identical(length(next_cursor(f)), 1L))
  expect_named(f, "user_id")
  expect_gt(NROW(f), 9999)
})
