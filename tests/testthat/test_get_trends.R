context("get_trends")


test_that("get_trends returns trends data", {
	skip_on_cran()
	skip_on_travis()

	x <- get_trends()

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(c("trend", "promoted_content") %in% names(x)))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 5)
	expect_equal(check_woeid("world"), 1)
	expect_equal(check_woeid("kansas"), 2430683)
	expect_equal(check_woeid("new york"), 2459115)
})