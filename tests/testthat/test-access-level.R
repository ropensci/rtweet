context("test-access-level")

test_that("api_access_level works", {
  if (!file.exists("twiter_tokens")) {
    token <- rtweet::create_token(
      "rstats2twitter",
      consumer_key = rtweet:::decript_key(),
      consumer_secret = rtweet:::decript_secret(),
      access_secret = rtweet:::rtweet_find_access_secret(),
      access_token = rtweet:::rtweet_find_access_key(),
      set_renv = FALSE
    )
    e <- tryCatch(rtweet:::api_access_level(token),
      error = function(e) NULL)
    saveRDS(token, "twitter_tokens")
  }
  token <- readRDS("twitter_tokens")
  a <- api_access_level(token)
  expect_true(is.character(a) && length(a) == 1)
})
