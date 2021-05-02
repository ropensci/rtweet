test_that("tweets_data works", {
  jack <- lookup_users("jack")

  ## get data on most recent tweet from user(s)
  tweets <- tweets_data(jack)
  expect_s3_class(tweets, "tbl_df")
  expect_true("status_id" %in% names(tweets))
})

test_that("users_data works", {
  tweets <- search_tweets("r")
  users <- users_data(tweets)
  expect_s3_class(users, "tbl_df")
})
