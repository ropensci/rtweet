skip_on_cran()

library(rtweet)

context("twitter_token")

load("twitter_token")

x <- search_tweets(
	q = "twitter",
	verbose = FALSE,
	token = twitter_token)

test_that("search_tweets with twitter_token returns tweets data", {
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
})

test_that("output contains > 10 rows and > 10 columns", {
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 10)
})
