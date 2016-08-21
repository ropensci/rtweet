skip_on_cran()
skip_on_travis()

library(rtweet)

context("get_trends")

x <- get_trends()

test_that("get_trends returns trends data", {
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(c("trend", "promoted_content") %in% names(x)))
})

test_that("trends data contains > 10 rows > 5 columns", {
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 5)
})

test_that("find_trends and check_trends work correctly", {
	expect_equal(check_woeid("world"), 1)
	expect_equal(check_woeid("kansas"), 2430683)
	expect_equal(check_woeid("new york"), 2459115)
})