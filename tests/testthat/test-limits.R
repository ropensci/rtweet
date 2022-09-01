test_that("pagination with rate limits works", {
  skip("requires manual testing")
  # One needs to wait 15 minutes...
  df_follower <- get_followers("CDU", n = 80000L, retryonratelimit = TRUE)
  expect_equal(nrow(df_follower), 80000L)
})
