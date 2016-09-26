context("trends_available")

test_that("trends_available returns data frame", {
	skip_on_cran()

	token <- readRDS("twitter_tokens")
	x <- trends_available(token = token)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(c("name", "woeid") %in% names(x)))
  expect_gt(nrow(x), 5)
  expect_gt(ncol(x), 2)
	expect_equal(as.integer(subset(x, name == "Worldwide", select = "woeid")), 1)
})
