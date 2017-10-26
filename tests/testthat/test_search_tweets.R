context("search_tweets")

test_that("search_tweets returns tweets data", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- search_tweets("tweet tweet", n = n, type = "recent",
                     token = token, lang = "en")

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(x$lang == "en"))
  expect_true(any(c("status_id", "text") %in% names(x)))
  expect_gt(nrow(x), 25)
  expect_gt(ncol(x), 15)
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))
  expect_true(any(c("user_id", "friends_count") %in% names(users_data(x))))

  x <- search_tweets("lol", n = 300,
                     include_rts = FALSE, token = token, lang = "en")

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(x$lang == "en"))
  expect_true(any(c("status_id", "text") %in% names(x)))
  expect_gt(nrow(x), 200)
  expect_gt(ncol(x), 15)
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))
  expect_true(any(c("user_id", "friends_count") %in% names(users_data(x))))
  expect_true(all(FALSE %in% x$is_retweet))

  expect_error(search_tweets(c(1:10), verbose = FALSE, token = token))
  expect_error(search_tweets("tweet", token = "token"))
  expect_error(search_tweets("stats", type = "all", token = token))
  expect_error(search_tweets("stats", type = "all", n = -1, token = token))

  expect_equal(nrow(users_data(letters)), 0L)

})
