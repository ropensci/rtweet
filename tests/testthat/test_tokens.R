context("get_tokens")


test_that("get_tokens returns tokens", {
	skip_on_cran()

	token <- readRDS("twitter_tokens")
	saveRDS(token, file = ".httr_oauth")
	tokens <- get_tokens()
	expect_true(is.list(tokens))
	expect_true("Token" %in% class(tokens[[1]]))

	lol <- search_tweets("lol")
	expect_true(is.data.frame(lol))
	expect_true(nrow(lol) > 80)
	file.remove(".httr_oauth")
})