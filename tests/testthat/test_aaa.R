context("setup token")

test_that("create token", {
  skip_on_cran()
  
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
  expect_true(!is.null(e))
  saveRDS(token, "twitter_tokens")
  expect_true(file.exists("twitter_tokens"))
})
