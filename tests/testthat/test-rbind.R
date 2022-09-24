test_that("do_call_rbind works", {
  ## lapply through three different search queries
  vcr::use_cassette("do_call_rbind", {
    lrt <- lapply( c("rstats OR tidyverse", "data science"), search_tweets)
  })

  rt <- do_call_rbind(lrt)

  expect_s3_class(rt, "tweets_with_users")
  expect_equal(nrow(rt), 200)
  expect_equal(nrow(users_data(rt)), 200)
})

test_that("rbind works", {
  ## lapply through three different search queries
  vcr::use_cassette("rbind", {
    lrt <- lapply( c("rstats OR tidyverse", "data science"), search_tweets)
  })

  rt <- rbind(lrt[[1]], lrt[[2]])

  expect_s3_class(rt, "tweets_with_users")
  expect_equal(nrow(rt), 200)
  expect_equal(nrow(users_data(rt)), 200)
})
