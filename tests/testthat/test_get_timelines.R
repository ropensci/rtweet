context("get_timeline")

test_that("get_timeline", {
  skip_on_cran()

  n <- 500
  token <- readRDS("twitter_tokens")
  x <- get_timeline("jack", n = n, token = token)
  xts <- ts_data(x, by = "hours")

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 200)
  expect_gt(ncol(x), 25)
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
  expect_true(is.data.frame(xts))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))
})
