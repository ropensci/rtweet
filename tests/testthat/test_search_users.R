context("search_users")

test_that("search_users returns users data", {
  skip_on_cran()
  skip_if_offline()

  n <- 3
  x <- search_users("twitter", n = n, verbose = FALSE)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
  expect_gt(nrow(x), 2)
  expect_gt(ncol(x), 15)
  expect_true(all(c("text", "retweet_count") %in% names(x)))
  #expect_true(is.data.frame(attr(x, "tweets")))
  #expect_true(is.data.frame(tweets_data(x)))
  #expect_gt(nrow(tweets_data(x)), 0)
  #expect_gt(ncol(tweets_data(x)), 15)
  #expect_named(tweets_data(x))
})
