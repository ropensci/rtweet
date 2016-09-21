context("get_tokens")


test_that("get_tokens returns tokens", {
	skip_on_cran()

	twitter_tokens <- readRDS("twitter_tokens")
	saveRDS(twitter_tokens, file = ".httr_oauth")
	tokens <- get_tokens()
	lol <- search_tweets("lol")
	file.remove(".httr_oauth")

	expect_true(is.list(tokens))
	expect_true("Token" %in% class(tokens[[1]]))
	expect_true(is.data.frame(lol))
	expect_true(nrow(lol) > 80)
})