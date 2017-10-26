context("save_as_csv")

test_that("save_as_csv saves tweets data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- search_tweets(q = "obama", token = token)
  save_as_csv(x, "csv_data")
  list_files <- list.files()

  expect_true(all(c("csv_data.tweets.csv", "csv_data.tweets.csv") %in% list_files))
  expect_true(is.data.frame(utils::read.csv("csv_data.tweets.csv")))
  expect_gt(nrow(utils::read.csv("csv_data.tweets.csv")), 60)
  expect_gt(ncol(utils::read.csv("csv_data.tweets.csv")), 15)
  expect_gt(nrow(utils::read.csv("csv_data.users.csv")), 50)
  expect_gt(ncol(utils::read.csv("csv_data.users.csv")), 15)
  unlink("csv_data.tweets.csv")
  unlink("csv_data.users.csv")
})
