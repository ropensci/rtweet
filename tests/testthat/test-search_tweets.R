test_that("search_tweets returns tweets data and latlng", {
  df <- search_tweets("#rstats", n = 50)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 25) # should almost always be true
  
  # can extract lat_lng
  ll <- lat_lng(df)
  expect_equal(c("lat", "lng") %in% names(ll), c(TRUE, TRUE))
})

test_that("gives useful errors", {
  expect_snapshot(search_tweets(c(1:10), verbose = FALSE), error = TRUE)
  expect_snapshot(search_tweets("stats", type = "all"), error = TRUE)
})

test_that("non-existent search returns empty data frame", {
  tweets <- search_tweets("abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc")
  expect_s3_class(tweets, "data.frame")
  expect_equal(nrow(tweets), 0)
})

test_that("search_tweets2 can search for multiple queries", {
  df <- search_tweets2(c("#rstats", "open science"), n = 50)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 25) # should almost always be true
})
