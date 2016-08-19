skip_on_cran()

library(rtweet)

context("Search users")

n <- 3
x <- search_users("twitter", n = n, verbose = FALSE)

test_that("search_users returns users data", {
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
})

test_that(paste0("users data contains ", n, " rows > 15 columns"), {
  expect_equal(nrow(x), n)
  expect_gt(ncol(x), 15)
})

test_that("search_users object contains tweets attribute", {
  expect_true("tweets" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "tweets")))
  expect_true(is.data.frame(tweets_data(x)))
})

test_that(paste0("tweets data contains > 0 rows and > 23 columns"), {
  expect_gt(nrow(tweets_data(x)), 0)
  expect_gt(ncol(tweets_data(x)), 23)
  expect_named(tweets_data(x))
})
