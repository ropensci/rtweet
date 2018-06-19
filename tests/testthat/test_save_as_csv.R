context("save_as_csv")

test_that("save_as_csv saves tweets data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- search_tweets(q = "obama", token = token)
  save_as_csv(x, "csv_data.csv")
  expect_gt(nrow(utils::read.csv("csv_data.csv")), 60)
  expect_gt(ncol(utils::read.csv("csv_data.csv")), 15)
  unlink("csv_data.csv")
  d <- flata(x)
  all(vapply(d, is.atomic, FUN.VALUE = logical(1)))
})
