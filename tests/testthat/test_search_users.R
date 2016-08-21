library(rtweet)

context("search_users")

skip_on_cran()
skip_on_travis()
n <- 3
x <- search_users("twitter", n = n, verbose = FALSE)

test_that("search_users returns users data", {
	skip_on_cran()
	skip_on_travis()
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
  expect_equal(nrow(x), n)
  expect_gt(ncol(x), 15)
  expect_true("tweets" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "tweets")))
  expect_true(is.data.frame(tweets_data(x)))
  expect_gt(nrow(tweets_data(x)), 0)
  expect_gt(ncol(tweets_data(x)), 23)
  expect_named(tweets_data(x))
})
