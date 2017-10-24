context("get_trends")


test_that("get_trends returns trends data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- get_trends(token = token)

  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true(all(c("trend", "promoted_content") %in% names(x)))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 5)
  expect_equal(check_woeid("world"), "1")
  expect_equal(check_woeid("kansas"), "2347575")
  expect_equal(check_woeid("new york"), "2347591")
})
