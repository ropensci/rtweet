test_that("can retrieve multiple users", {
  users <- c("hadleywickham", "jennybryan")
  
  out <- get_favorites(users, n = 20)
  expect_s3_class(out, "tbl_df")
  expect_s3_class(out$created_at, "POSIXct")
  expect_equal(out$favorited_by, rep(users, each = 20))
})
