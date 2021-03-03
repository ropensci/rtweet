test_that("search_tweets returns tweets data and latlng", {
  skip_on_cran()
  skip_if_offline()

  df <- search_tweets("#rstats", n = 50)
  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 25) # should almost always be true
  
  # can extract lat_lng
  ll <- lat_lng(df)
  expect_equal(c("lat", "lng") %in% names(ll), c(TRUE, TRUE))
})

test_that("gives useful errors", {
  expect_snapshot(search_tweets(c(1:10), verbose = FALSE), error = TRUE)
  expect_snapshot(search_tweets("stats", type = "all"), error = TRUE)
})
test_that("search_tweets2 returns tweets data and latlng", {
  skip_on_cran()
  skip_if_offline()

  df <- search_tweets2(c("#rstats", "open science"), n = 50)
  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 25) # should almost always be true
  
  # can extract lat_lng
  ll <- lat_lng(df)
  expect_equal(c("lat", "lng") %in% names(ll), c(TRUE, TRUE))
})

