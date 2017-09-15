context("stream_tweets")

test_that("stream_tweets returns tweets data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- suppressMessages(
    stream_tweets(paste(letters, collapse = ","),
                  timeout = 4, gzip = TRUE, verbose = FALSE, token = token))

  x <- suppressMessages(
    stream_tweets(paste(letters, collapse = ","),
                  timeout = 4, verbose = TRUE, token = token))

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 0)
  expect_gt(ncol(x), 15)
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))
})
