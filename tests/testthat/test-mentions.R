test_that("mentions returns tweets data", {
  suppressMessages(x <- get_mentions())

  expect_s3_class(x, "tbl_df")
  expect_gt(nrow(x), 0)
})
