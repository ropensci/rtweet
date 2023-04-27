
write_thread <- function() {
  pt1 <- suppressMessages(post_tweet(status = paste0("first in a thread", Sys.time())))
  pt2 <- suppressMessages(post_tweet(paste0("second in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt1)))
  pt3 <- suppressMessages(post_tweet(paste0("3rd in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt2)))
  pt4 <- suppressMessages(post_tweet(paste0("4th in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt3)))
  pt5 <- suppressMessages(post_tweet(paste0("5th in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt4)))
  pt6 <- suppressMessages(post_tweet(paste0("6th in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt5)))
  pt7 <- suppressMessages(post_tweet(paste0("7th in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt6)))
  pt8 <- suppressMessages(post_tweet(paste0("8th in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt7)))
  pt9 <- suppressMessages(post_tweet(paste0("9th in the thread", Sys.time()),
                                     in_reply_to_status_id = ids(pt8)))
  pt10 <- suppressMessages(post_tweet(paste0("10th in the thread", Sys.time()),
                                      in_reply_to_status_id = ids(pt9)))
  c(ids(pt1), ids(pt2), ids(pt3), ids(pt4), ids(pt5),
    ids(pt6), ids(pt7), ids(pt8), ids(pt9), ids(pt10))
}

delete_thread <- function(ids) {
  for (id in ids) {
    suppressMessages(post_destroy(id))
  }
}

test_that("tweet_threading works", {
  skip_if_offline()

  tryCatch(thread <- write_thread(), error = function(x){
    skip("Not able to post.")
  })
  tw <- lookup_tweets(thread[1])
  tw_thread <- tweet_threading(tw)
  expect_s3_class(tw_thread, "data.frame")

  tw <- lookup_tweets(thread)
  tw_thread1 <- tweet_threading(tw[1, ])
  expect_s3_class(tw_thread1, "data.frame")
  tw_thread2 <- tweet_threading(tw[2, ])
  expect_s3_class(tw_thread2, "data.frame")
  tw_thread3 <- tweet_threading(tw[3, ])
  expect_s3_class(tw_thread3, "data.frame")
  tw_thread4 <- tweet_threading(tw[4, ])
  expect_s3_class(tw_thread4, "data.frame")
  expect_equal(order(tw_thread1$created_at)[1:4], 1:4)
  expect_equal(order(tw_thread4$created_at)[1:4], 1:4)

  tw <- tw[match(thread, tw$id_str), ]

  th1_id <- tweet_threading(thread[1], traverse = "backwards")
  th1_tw <- tweet_threading(tw[1, ], traverse = "backwards")
  expect_equal(nrow(th1_id), nrow(th1_tw))
  th10_id <- tweet_threading(thread[10], traverse = "backwards")
  th10_tw <- tweet_threading(tw[10, ], traverse = "backwards")
  expect_equal(nrow(th10_id), nrow(th10_tw))

  tw <- tw[match(thread, tw$id_str), ]
  th1_id <- tweet_threading(thread[1], traverse = "forwards")
  expect_equal(nrow(th1_id), 10)
  th1_tw <- tweet_threading(tw[1, ], traverse = "forwards")
  expect_equal(nrow(th1_id), nrow(th1_tw))

  th10_id <- tweet_threading(thread[10], traverse = "forwards")
  expect_equal(nrow(th10_id), 1)
  th10_tw <- tweet_threading(tw[10, ], traverse = "forwards")
  expect_equal(nrow(th10_id), nrow(th10_tw))

  t1f <- tweet_threading(thread[1], traverse = "forwards")
  expect_equal(nrow(t1f), 10)
  t1b <- tweet_threading(thread[1], traverse = "backwards")
  expect_equal(nrow(t1b), 1)
  t10f <- tweet_threading(thread[10], traverse = "forwards")
  expect_equal(nrow(t10f), 1)
  t10b <- tweet_threading(thread[10], traverse = "backwards")
  expect_equal(nrow(t10b), 10)
  delete_thread(thread)
})
