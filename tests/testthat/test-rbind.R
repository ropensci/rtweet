test_that("do_call_rbind works", {
  ## lapply through three different search queries
  vcr::use_cassette("do_call_rbind", {
    lrt <- lapply( c("rstats OR tidyverse", "data science"), search_tweets, n = 10)
  })

  rt <- do_call_rbind(lrt)
  nrows <- vapply(lrt, nrow, numeric(1L))
  expect_s3_class(rt, "tweets")
  expect_equal(nrow(rt), sum(nrows))
  expect_equal(nrow(users_data(rt)), sum(nrows))
})

test_that("rbind works", {
  ## lapply through three different search queries
  vcr::use_cassette("rbind", {
    lrt <- lapply( c("rstats OR tidyverse", "data science"), search_tweets, n = 10)
  })

  nrows <- vapply(lrt, nrow, numeric(1L))
  rt <- rbind(lrt[[1]], lrt[[2]])

  expect_s3_class(rt, "tweets")
  expect_equal(nrow(rt), sum(nrows))
  expect_equal(nrow(users_data(rt)), sum(nrows))
})
