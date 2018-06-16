context("get_timeline")

test_that("get_timeline", {
  skip_on_cran()

  n <- 300
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("cnnbrk", "cnn"), n = n, token = token)
  xts <- ts_data(x, by = "hours", trim = 1)
  p <- ts_plot(xts)
  expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
  #p <- ts_plot(group_by(x, screen_name, is_retweet), "hours")
  #expect_true(inherits(p, "ggplot"))
  #}
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  expect_true(is.data.frame(users_data(x)))
  expect_true(is.data.frame(xts))
  unlink("Rplots.pdf")
})
