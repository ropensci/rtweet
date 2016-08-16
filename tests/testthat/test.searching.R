skip_on_cran()

library(rtweet)

context("Search tweets")

n <- 25
srch <- search_tweets(
  "twitter", n = n,
  lang = "en",
  verbose = FALSE)

test_that("search_tweets returns two data frames", {
  expect_equal(length(srch), 2)
  expect_named(srch, c("tweets", "users"))
})

tweets <- srch[["tweets"]]
users <- srch[["users"]]

test_that(paste0("tweets df contains ", n, " rows and 27 columns"), {
  expect_equal(nrow(tweets), n)
  expect_gt(ncol(tweets), 23)
  expect_named(tweets)
  expect_equal(unique(getElement(tweets, "lang")), "en")
})

test_that(paste0("users df contains > 1 rows and 19 columns"), {
  expect_gt(nrow(users), 0)
  expect_gt(ncol(users), 15)
  expect_named(users)
})
