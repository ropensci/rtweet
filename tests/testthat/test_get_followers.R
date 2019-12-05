context("get_followers")

test_that("get_followers returns data frame with user_id", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  rl <- rate_limit(token, "get_followers")
  if (rl$remaining > 1) {
    f <- get_followers("HillaryClinton", n = 10000, token = token)
  
    expect_true(is.data.frame(f))
    expect_true(identical(length(next_cursor(f)), 1L))
    expect_named(f, "user_id")
    expect_gt(NROW(f), 9999)
  } else {
    expect_true(rl$limit == 15)
    expect_true(rl$remaining == 0)
    expect_true(rl$limit == 15)
    expect_true(rl$limit == 15)
  }
})
