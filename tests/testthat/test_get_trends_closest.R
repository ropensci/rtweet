context("trends_available")

test_that("get_trends_closest returns data frame with correct city name", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- get_trends(lat = 40.7, lng = -74.0, token = token)

  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true(all(c("trend", "woeid") %in% names(x)))
  expect_gt(nrow(x), 5)
  expect_gt(ncol(x), 2)
  expect_equal(as.character(x[1,]$place), "New York")
})
