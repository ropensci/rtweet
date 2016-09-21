context("get_tokens")


test_that("get_tokens returns tokens", {
	skip_on_cran()

	token <- readRDS("twitter_tokens")
	saveRDS(token, file = "~/twitter_tokens.rds")
	tokens <- get_tokens()
	expect_true(is.list(tokens))
	expect_true("Token" %in% class(tokens[[1]]))

	Sys.setenv(TWITTER_PAT = "~/twitter_tokens.rds")
	lol <- search_tweets("lol")
	expect_true(is.data.frame(lol))
	expect_true(nrow(lol) > 80)
})