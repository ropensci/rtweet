test_that("rate_limit works", {
  rl <- rate_limit()
  expect_s3_class(rl, "tbl_df")
})
test_that("rate_limit works", {
  rl <- rate_limit("application/rate_limit_status")
  expect_s3_class(rl, "tbl_df")
  expect_equal(nrow(rl), 1)
})


test_that("rate_limit_reset works", {
  reset <- rate_limit_reset("application/rate_limit_status")
  expect_s3_class(reset, "POSIXct")
})
test_that("rate_limit_wait works", {
  wait <- rate_limit_wait("application/rate_limit_status")
  expect_null(wait)
})
