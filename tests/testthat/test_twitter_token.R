context("twitter_token")

test_that("search_tweets with twitter_token returns tweets data", {
	skip_on_cran()

	token <- readRDS("twitter_tokens")
	x <- search_tweets("twitter", token = token)

	expect_equal(is.data.frame(x), TRUE)
	expect_named(x)
	expect_true("status_id" %in% names(x))
	expect_gt(nrow(x), 10)
	expect_gt(ncol(x), 10)
	expect_true("users" %in% names(attributes(x)))
	expect_true(is.data.frame(attr(x, "users")))
	expect_true(is.data.frame(users_data(x)))
	expect_named(users_data(x))
	expect_gt(nrow(users_data(x)), 5)
	expect_gt(ncol(users_data(x)), 5)
})