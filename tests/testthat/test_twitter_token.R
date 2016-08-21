skip_on_cran()

library(rtweet)

context("twitter_token")

x <- search_tweets(
	q = "twitter",
	verbose = FALSE)

test_that("search_tweets with twitter_token returns tweets data", {
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
})

test_that("output contains > 10 rows and > 10 columns", {
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 10)
})

test_that("search_tweets object contains users attribute", {
	expect_true("users" %in% names(attributes(x)))
	expect_true(is.data.frame(attr(x, "users")))
	expect_true(is.data.frame(users_data(x)))
})

test_that("users data contains > 5 rows and > 5 columns", {
	expect_named(users_data(x))
	expect_gt(nrow(users_data(x)), 5)
	expect_gt(ncol(users_data(x)), 5)
})