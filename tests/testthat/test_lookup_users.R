skip_on_cran()

library(rtweet)

context("Lookup users")

n <- 4
x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))

test_that("lookup_users returns users data", {
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
})

test_that(paste0("users data contains ", n, " rows and 19 columns"), {
  expect_equal(nrow(x), n)
  expect_gt(ncol(x), 15)
})

test_that("lookup_users object contains tweets attribute", {
  expect_true("tweets" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "tweets")))
  expect_true(is.data.frame(users_data(x)))
})

test_that(paste0("tweets data contains > 0 rows and 27 columns"), {
  expect_gt(nrow(tweets_data(x)), 0)
  expect_gt(ncol(tweets_data(x)), 23)
  expect_named(tweets_data(x))
})
