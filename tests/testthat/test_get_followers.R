context("get_followers")

test_that("get_followers returns data frame with ids", {
	skip_on_cran()

	token <- readRDS("twitter_tokens")
	f <- get_followers("HillaryClinton", n = 10000, token = token)

  expect_true(is.data.frame(f))
  expect_named(f, "ids")
  expect_equal(nrow(f), 10000)
})
