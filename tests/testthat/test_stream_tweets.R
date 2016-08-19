skip_on_cran()

library(rtweet)

context("Stream tweets")

x <- stream_tweets("lol", timeout = 2, verbose = FALSE)

test_that("stream_tweets returns tweets data", {
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
})

test_that("tweets data contains > 0 rows and > 23 columns", {
  expect_gt(nrow(x), 0)
  expect_gt(ncol(x), 23)
})

test_that("stream_tweets returns users attribute", {
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
})

test_that(paste0("users data contains > 0 rows and > 15 columns"), {
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))
})
