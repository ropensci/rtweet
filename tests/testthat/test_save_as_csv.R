context("save_as_csv")

test_that("save_as_csv saves tweets data", {
	skip_on_cran()

	token <- readRDS("twitter_tokens")
	x <- search_tweets(q = "obama", token = token)
	save_as_csv(x, "csv_data.csv")
	list_files <- list.files()

	expect_true("csv_data.csv" %in% list_files)
	expect_true(is.data.frame(read.csv("csv_data.csv")))
	expect_gt(nrow(read.csv("csv_data.csv")), 95)
	expect_gt(ncol(read.csv("csv_data.csv")), 23)

	unlink("csv_data.csv")
})
