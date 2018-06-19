context("save_as_csv")

test_that("save_as_csv saves tweets data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- search_tweets(q = "obama", token = token)
  write_as_csv(x, "csv_data.csv", prepend_ids = FALSE)
  expect_gt(ncol(utils::read.csv("csv_data.csv")), 15)
  save_as_csv(x, "csv_data.csv")
  expect_gt(nrow(utils::read.csv("csv_data.csv")), 60)
  unf <- read_twitter_csv("csv_data.csv")
  expect_gt(ncol(unf), 15)
  expect_true(all(vapply(unf, is.atomic, FUN.VALUE = logical(1))))
  unf <- read_twitter_csv("csv_data.csv", unflatten = TRUE)
  expect_gt(ncol(unf), 15)
  expect_true(any(vapply(unf, is.recursive, FUN.VALUE = logical(1))))
  unlink("csv_data.csv")
})
