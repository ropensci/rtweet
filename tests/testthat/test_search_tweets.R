context("search_tweets")

test_that("search_tweets returns tweets data", {
  skip_on_cran()

  n <- 3
  token <- readRDS("twitter_tokens")
  x <- search_tweets("tweet tweet", n = n,
    lang = "en", verbose = TRUE, token = token)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(x$lang == "en"))
  expect_true("status_id" %in% names(x))
  expect_equal(nrow(x), n)
  expect_gt(ncol(x), 15)
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))

  expect_error(search_tweets(verbose = FALSE, token = token))
  expect_error(search_tweets("tweet", token = "token"))
  expect_error(search_tweets("stats", type = "all", token = token))
})
