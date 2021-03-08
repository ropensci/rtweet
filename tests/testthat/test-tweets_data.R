test_that("tweets_data works", {
  jack <- lookup_users("jack")

  ## get data on most recent tweet from user(s)
  tweets <- tweets_data(jack)
  expect_s3_class(tweets, "tbl")
  expect_equal(colnames(tweets), tweets_names())
})
