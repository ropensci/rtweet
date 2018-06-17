context("get_timeline")

test_that("get_timeline", {
  skip_on_cran()

  n <- 400
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("cnnbrk", "cnn"), n = n, token = token)
  expect_true(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  expect_true(inherits(p, "ggplot"))
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(is.data.frame(g), TRUE)
  expect_equal(ncol(g), 4)
  expect_true(inherits(p, "ggplot"))
  unlink("Rplots.pdf")
})
