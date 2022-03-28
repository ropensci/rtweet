test_that("tweet_threading works", {
  
  vcr::use_cassette("tweet_threading", {
    tw <- lookup_tweets('1461776330584956929')
    tw_thread <- tweet_threading(tw)
  })
  expect_s3_class(tw_thread, "data.frame")
})


test_that("tweet_threading works fast", {
  
  thread_ids <- c("1508513828593672192", "1508513830372007936", 
                  "1508513831974232077", "1508513833580699652")
  vcr::use_cassette("tweet_threading2", {
    tw <- lookup_tweets(thread_ids)
    tw_thread1 <- tweet_threading(tw[1, ])
    tw_thread2 <- tweet_threading(tw[2, ])
    tw_thread3 <- tweet_threading(tw[3, ])
    tw_thread4 <- tweet_threading(tw[4, ])
  })
  expect_s3_class(tw_thread1, "data.frame")
  expect_s3_class(tw_thread2, "data.frame")
  expect_s3_class(tw_thread3, "data.frame")
  expect_s3_class(tw_thread4, "data.frame")
  expect_equal(order(tw_thread1$created_at), 1:4)
  expect_equal(order(tw_thread4$created_at), 1:4)
})
