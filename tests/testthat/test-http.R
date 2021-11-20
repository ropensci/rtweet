test_that("TWIT_paginte_max_id respects max_id and since_id", {
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
  expect_true(min(format_date(older$created_at)) < min(format_date(base$created_at)))

  # asking for newer tweets should give back the original data
  base2 <- simple_timeline(since_id = older)
  expect_length(intersect(base$id, base2$id), nrow(base))
})  

test_that("TWIT_paginte_cursor respects cursor", {
  page1 <- get_followers("JustinBieber")
  page2 <- get_followers("JustinBieber", cursor = page1)
  
  expect_length(intersect(page1$user_id, page2$user_id), 0)
})  
