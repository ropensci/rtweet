skip_on_cran()

library(rtweet)

context("Get timelines")

n <- 25
d <- get_timeline("kearneymw", n = n)

test_that("get_timeline returns two data frames", {
  expect_equal(length(d), 2)
  expect_named(d, c("tweets", "users"))
})

tweets <- d[["tweets"]]
users <- d[["users"]]

test_that(paste0("tweets df contains ", n, " rows and > 23 columns"), {
  expect_equal(nrow(tweets), n)
  expect_gt(ncol(tweets), 23)
  expect_named(tweets)
})

test_that(paste0("users df contains > 1 rows and 19 columns"), {
  expect_gt(nrow(users), 0)
  expect_gt(ncol(users), 15)
  expect_named(users)
})
