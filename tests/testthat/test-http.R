test_that("multiplication works", {
  simple_timeline <- function(...) {
    r <- TWIT_paginate_max_id(NULL, "/1.1/statuses/user_timeline", 
      list(screen_name = "JustinBieber"), 
      n = 100,
      ...
    )
    tweets_with_users(r)[1:10]
  }

  base <- simple_timeline()
  
  # check that we can ask for older tweets
  older <- simple_timeline(max_id = base)
  expect_true(min(older$created_at) < min(base$created_at))

  # asking for newer tweets should give back the original data
  base2 <- simple_timeline(since_id = older)
  expect_length(intersect(base$status_id, base2$status_id), 100)
})  
