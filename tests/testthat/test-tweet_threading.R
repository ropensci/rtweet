test_that("tweet_threading works", {
  tw <- lookup_tweets('1084143184664510470')
  tw_thread <- tweet_threading(tw)
  expect_s3_class(tw_thread, "data.frame")
})
