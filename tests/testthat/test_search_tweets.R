context("search_tweets")

test_that("search_tweets returns tweets data and latlng", {
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
  expect_true(all(c("friends_count", "description") %in% names(x)))
  expect_true(any(c("user_id", "friends_count") %in% names(x)))

  x <- search_tweets("lol", n = 300,
                     include_rts = FALSE, token = token, lang = "en")
  x <- lat_lng(x)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(x$lang == "en"))
  expect_true(any(c("status_id", "text") %in% names(x)))
  expect_gt(nrow(x), 200)
  expect_gt(ncol(x), 15)
  expect_true(all(c("friends_count", "description") %in% names(x)))
  expect_true(all(c("lat", "lng") %in% names(x)))
  expect_true(any(c("user_id", "friends_count") %in% names(x)))
  expect_true(all(FALSE %in% x$is_retweet))

  expect_error(search_tweets(c(1:10), verbose = FALSE, token = token))
  expect_error(search_tweets("tweet", token = "token"))
  expect_error(search_tweets("stats", type = "all", token = token))
  expect_error(search_tweets("stats", type = "all", n = -1, token = token))

  expect_equal(nrow(users_data(letters)), 0L)

})
