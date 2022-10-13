test_that("[.tweets works", {
  ## lapply through three different search queries
  vcr::use_cassette("[.tweets", {
    st <- search_tweets("#rstats")
  })
  nrows <- 5
  x <- expect_error(st[seq_len(nrows), ], NA)

  expect_s3_class(x, "tweets")
  expect_equal(nrow(x), nrows)
  expect_equal(nrow(users_data(x)), nrows)

  # Not subsetting columns in the users data
  y <- expect_error(st[seq_len(nrows), 1:3], NA)
  expect_equal(ncol(y), 3)
  expect_equal(ncol(users_data(y)), 23)

  y <- st[c(1, 2), "full_text", drop = TRUE]
  expect_true(is.character(y))
  expect_length(y, 2)
  expect_true(is.null(attr(y, "users")))
  y <- st[c(1, 2), "full_text"]
  expect_s3_class(y, "data.frame")
  expect_length(y, 1)
  expect_true(!is.null(attr(y, "users")))
})

test_that("[.users works", {
  ## lapply through three different search queries
  vcr::use_cassette("[.users", {
    st <- search_users("#rstats")
  })
  nrows <- 5
  x <- expect_error(st[seq_len(nrows), ], NA)

  expect_s3_class(x, "users")
  expect_equal(nrow(x), nrows)
  expect_equal(nrow(tweets_data(x)), nrows)

  y <- expect_error(st[seq_len(nrows), 1:3], NA)
  expect_equal(ncol(y), 3)
  expect_equal(ncol(tweets_data(y)), ncol(tweet(NULL)) - 1)

  y <- st[c(1, 2), "name", drop = TRUE]
  expect_true(is.character(y))
  expect_length(y, 2)
  expect_true(is.null(attr(y, "tweets")))
  y <- st[c(1, 2), "name"]
  expect_s3_class(y, "data.frame")
  expect_length(y, 1)
  expect_true(!is.null(attr(y, "tweets")))
})
