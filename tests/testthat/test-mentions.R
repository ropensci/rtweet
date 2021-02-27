test_that("mentions returns tweets data", {
  skip_on_cran()
  skip_if_offline()

  x <- get_mentions()

  expect_s3_class(x, "tbl_df")
  expect_gt(nrow(x), 0)
})
