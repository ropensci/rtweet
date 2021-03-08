test_that("get_my_timeline works", {
  gmt <- get_my_timeline()
  expect_s3_class(gmt, "tbl_df")
  expect_equal(nrow(gmt), 100)
})
