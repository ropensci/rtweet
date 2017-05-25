context("trends_available")

test_that("get_trends_closest returns data frame with correct city name", {
  skip_on_cran()
  
  token <- readRDS("twitter_tokens")
  x <- get_trends_closest(lat = 40.7, long = -74.0, token = token)
  
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(c("trend", "woeid") %in% names(x)))
  expect_gt(nrow(x), 5)
  expect_gt(ncol(x), 2)
  expect_equal(as.character(x[1,]$place), "New York")
})
