test_that("lookup_users returns users data", {
  skip_on_cran()
  skip_if_offline()

  x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 4)
})
