
context("friendships")

test_that("friendships returns data", {
  skip_on_cran()
  token <- readRDS("twitter_tokens")

  x <- my_friendships("kearneymw", token = token)
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("screen_name" %in% names(x))

  x <- lookup_friendships("kearneymw", c("realdonaldtrump", "cstonehoops"))

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("relationship" %in% names(x))
})

