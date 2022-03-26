test_that("tweet_threading works", {
  
  vcr::use_cassette("tweet_threading", {
    tw <- lookup_tweets('1461776330584956929')
    tw_thread <- tweet_threading(tw)
  })
  expect_s3_class(tw_thread, "data.frame")
})
