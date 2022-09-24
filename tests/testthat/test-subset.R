test_that("[.tweets_with_users works", {
  ## lapply through three different search queries
  vcr::use_cassette("[.tweets_with_users", {
    st <- search_tweets("#rstats")
  })
  nrows <- 5
  x <- expect_error(st[seq_len(nrows), ], NA)

  expect_s3_class(x, "tweets_with_users")
  expect_equal(nrow(x), nrows)
  expect_equal(nrow(users_data(x)), nrows)

  # Not subsetting columns in the users data
  y <- expect_error(st[seq_len(nrows), 1:3], NA)
  expect_equal(ncol(y), 3)
  expect_equal(ncol(users_data(y)), 23)
})

test_that("[.users_with_tweets works", {
  ## lapply through three different search queries
  vcr::use_cassette("[.users_with_tweets", {
    st <- search_users("#rstats")
  })
  nrows <- 5
  x <- expect_error(st[seq_len(nrows), ], NA)

  expect_s3_class(x, "users_with_tweets")
  expect_equal(nrow(x), nrows)
  expect_equal(nrow(tweets_data(x)), nrows)

  y <- expect_error(st[seq_len(nrows), 1:3], NA)
  expect_equal(ncol(y), 3)
  expect_equal(ncol(tweets_data(y)), 44)
})
