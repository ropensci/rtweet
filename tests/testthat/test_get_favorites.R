context("get_favorites")

test_that("get_favorites returns tweets data", {
  skip_on_cran()

  n <- 100
  token <- readRDS("twitter_tokens")
  x <- get_favorites("kearneymw", n = n, token = token)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 15)
  expect_true(is.data.frame(data.frame(users_data(x))))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))
})
