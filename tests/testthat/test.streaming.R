skip_on_cran()

library(rtweet)

context("Stream tweets")

strm <- stream_tweets("r", timeout = 3, verbose = FALSE)

test_that("stream_tweets returns two data frames", {
  expect_equal(length(strm), 2)
  expect_named(strm, c("tweets", "users"))
})

tweets <- strm[["tweets"]]
users <- strm[["users"]]

test_that("tweets df contains > 0 rows and 27 columns", {
  expect_gt(nrow(tweets), 0)
  expect_gt(ncol(tweets), 23)
  expect_named(tweets)
})

test_that(paste0("users df contains > 0 rows and 19 columns"), {
  expect_gt(nrow(users), 0)
  expect_gt(ncol(users), 15)
  expect_named(users)
})
