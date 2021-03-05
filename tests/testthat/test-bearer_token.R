test_that("can create and invalidate a bearer token", {
  t <- get_token()
  bt <- bearer_token(t$app$key, t$app$secret)
  expect_s3_class(bt, "rtweet_bearer")
  
  # We can use it for searching
  expect_error(search_tweets("tweet", n = 10, token = bt), NA)
  # But not for user info
  expect_error(api_screen_name(bt), class = "rtweet_error_http")
  
  invalidate_bearer(bt, t)
  # We can't use it at all after invalidating
  expect_error(search_tweets("tweet", n = 10, token = bt), class = "rtweet_error_http")
})
