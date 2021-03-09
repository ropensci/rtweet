test_that("lookup_users returns users data", {
  x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 4)
})
