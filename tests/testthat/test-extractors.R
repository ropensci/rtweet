test_that("tweets_data works", {
  
  vcr::use_cassette("tweet_data1", {
    jack <- lookup_users("jack")
  })

  ## get data on most recent tweet from user(s)
  tweets <- tweets_data(jack)
  expect_s3_class(tweets, "data.frame")
  expect_true("id_str" %in% names(tweets))
})

test_that("users_data works", {
  
  vcr::use_cassette("tweet_data2", {
    tweets <- search_tweets("r")
  })
  users <- users_data(tweets)
  expect_s3_class(users, "data.frame")
})
