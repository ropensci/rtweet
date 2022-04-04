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


test_that("tweet_threading works with id and tweets", {
  
  threads <- c("1256278643224965120", "1256278643883442176", 
                  "1256278644508377088", "1256278645255008256", 
                  "1256278645905055745", "1256278646605504512", 
                  "1256278647226265600", "1256278647914156033", 
                  "1256278648690262018", "1256278649600225280")
  
  vcr::use_cassette("tweet_threading3", {
    tw <- lookup_tweets(threads)
    tw <- tw[match(threads, tw$id_str), ]
    th1_id <- tweet_threading(threads[1], traverse = "backwards")
    th1_tw <- tweet_threading(tw[1, ], traverse = "backwards")
    th10_id <- tweet_threading(threads[10], traverse = "backwards")
    th10_tw <- tweet_threading(tw[10, ], traverse = "backwards")
  })
  
  expect_equal(nrow(th1_id), nrow(th1_tw))
  expect_equal(nrow(th10_id), nrow(th10_tw))
})

test_that("tweet_threading works forwards", {
  
  threads <- c("1256278643224965120", "1256278643883442176", 
                  "1256278644508377088", "1256278645255008256", 
                  "1256278645905055745", "1256278646605504512", 
                  "1256278647226265600", "1256278647914156033", 
                  "1256278648690262018", "1256278649600225280")
  
  vcr::use_cassette("tweet_threading4", {
    tw <- lookup_tweets(threads)
    tw <- tw[match(threads, tw$id_str), ]
    th1_id <- tweet_threading(threads[1], traverse = "forwards")
    th1_tw <- tweet_threading(tw[1, ], traverse = "forwards")
    th10_id <- tweet_threading(threads[10], traverse = "forwards")
    th10_tw <- tweet_threading(tw[10, ], traverse = "forwards")
  })
  
  expect_equal(nrow(th1_id), 10)
  expect_equal(nrow(th10_id), 1)
  expect_equal(nrow(th1_id), nrow(th1_tw))
  expect_equal(nrow(th10_id), nrow(th10_tw))
})

test_that("tweet_threading works forwards and backwards", {
  
  threads <- c("1256278643224965120", "1256278643883442176", 
               "1256278644508377088", "1256278645255008256", 
               "1256278645905055745", "1256278646605504512", 
               "1256278647226265600", "1256278647914156033", 
               "1256278648690262018", "1256278649600225280")
  
  vcr::use_cassette("tweet_threading5", {
    
    t1f <- tweet_threading(threads[1], traverse = "forwards")
    t1b <- tweet_threading(threads[1], traverse = "backwards")
    t10f <- tweet_threading(threads[10], traverse = "forwards")
    t10b <- tweet_threading(threads[10], traverse = "backwards")
  })
  expect_equal(nrow(t1f), 10)
  expect_equal(nrow(t1b), 1)
  expect_equal(nrow(t10f), 1)
  expect_equal(nrow(t10b), 10)
})
