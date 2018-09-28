context("test-access-level")

test_that("api_access_level works", {
  token <- readRDS("twitter_tokens")
  a <- api_access_level(token)
  expect_true(is.character(a), length(a) == 1)
})
