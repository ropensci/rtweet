context("test-test_username-r")

test_that("test authenticating user name", {
  skip_on_cran()
  token <- readRDS("twitter_tokens")
  sn <- rtweet:::authenticating_user_name(token)
  expect_equal(sn, "kearneymw")
})
