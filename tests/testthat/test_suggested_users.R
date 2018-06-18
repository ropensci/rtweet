
context("suggested_users")

test_that("all_suggested_users returns users data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  slugs <- suggested_slugs()
  x <- suggested_users_all(slugs[1:3, ], token = token)

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
  expect_gt(nrow(x), 2)
  expect_gt(ncol(x), 6)
})
