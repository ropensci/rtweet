test_that("get_followers returns data frame with user_id", {
  skip_on_cran()
  skip_if_offline()

  f <- get_followers("HillaryClinton", n = 10000)

  expect_s3_class(f, "tbl_df")
  expect_named(f, "user_id")
  expect_gt(NROW(f), 9999)
})
