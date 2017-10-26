context("system_token")

test_that("system_token functions", {
  skip_on_cran()

  x <- search_tweets(
    "a OR b OR c OR d OR e",
    max_id = NULL,
    type = "recent",
    include_rts = TRUE
  )

  tokens <- get_tokens()
  x <- search_tweets(
    "a OR b OR c OR d OR e",
    token = tokens,
    max_id = NULL,
    type = "recent",
    include_rts = FALSE
  )
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(ncol(x), 15)
  expect_gt(nrow(x), 50)
  expect_true("users" %in% names(attributes(x)))
  expect_true(is.data.frame(attr(x, "users")))
  expect_true(is.data.frame(users_data(x)))
  expect_gt(nrow(users_data(x)), 0)
  expect_gt(ncol(users_data(x)), 15)
  expect_named(users_data(x))

  expect_error(search_tweets(c(1:10), verbose = FALSE, token = tokens))
  expect_error(search_tweets("tweet", token = "token"))
  expect_error(search_tweets("stats", type = "all", token = tokens))
})
