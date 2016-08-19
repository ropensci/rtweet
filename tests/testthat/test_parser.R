library(rtweet)

context("Parse tweets")

d <- readr::read_rds("twitter_data")
d <- rtweet_parser(d)

test_that("rtweet_parser returns two data frames", {
  expect_equal(length(d), 2)
  expect_named(d, c("tweets", "users"))
})

tweets <- d[["tweets"]]
users <- d[["users"]]

test_that("tweets df contains 397 rows and 27 columns", {
  expect_equal(nrow(tweets), 397)
  expect_equal(ncol(tweets), 27)
  expect_named(tweets)
})

test_that(paste0("users df contains 396 rows and 19 columns"), {
  expect_equal(nrow(users), 396)
  expect_equal(ncol(users), 19)
  expect_named(users)
})
