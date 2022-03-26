test_that("search_tweets returns tweets data and latlng", {
  skip_if_offline()
  vcr::use_cassette("search_tweets", {
    df <- search_tweets("#rstats", n = 50)
  })
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
  skip_if_offline()
  vcr::use_cassette("search_tweets1", {
    tweets <- search_tweets("abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc")
  })
  expect_s3_class(tweets, "data.frame")
  expect_equal(nrow(tweets), 0)
})

test_that("search_tweets2 can search for multiple queries", {
  skip_if_offline()
  vcr::use_cassette("search_tweets2", {
    df <- search_tweets2(c("#rstats", "open science"), n = 50)
  })
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 25) # should almost always be true
})

test_that("search_tweets uses POSIXct at created_at, #660", {
  skip_if_offline()
  vcr::use_cassette("search_tweets3", {
    df <- search_tweets2(c("#rstats", "open science"), n = 50)
  })
  expect_true(is(df$created_at, "POSIXct"))
})
