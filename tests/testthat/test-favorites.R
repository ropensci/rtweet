test_that("can retrieve multiple users", {
  users <- c("hadleywickham", "jennybryan")
  
  out <- get_favorites(users, n = 20)
  expect_s3_class(out, "tbl_df")
  expect_s3_class(out$created_at, "POSIXct")
  expect_equal(out$favorited_by, rep(users, each = 20))
})

test_that("get_favorites returns tweets data", {
  n <- 100
  x <- get_favorites("kearneymw", n = n)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 15)
  expect_true(is.data.frame(data.frame(users_data(x))))
  #expect_gt(nrow(users_data(x)), 0)
  #expect_gt(ncol(users_data(x)), 15)
  #expect_named(users_data(x))
})
