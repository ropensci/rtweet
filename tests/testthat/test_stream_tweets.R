context("stream_tweets")

test_that("stream_tweets returns tweets data", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- suppressMessages(
    stream_tweets(paste(letters, collapse = ","),
                  timeout = 4, verbose = TRUE, token = token))

  x <- suppressMessages(
    stream_tweets(paste(letters, collapse = ","),
                  verbose = FALSE,
                  file_name = "tmp.json", parse = FALSE,
                  timeout = 4, token = token))
  x <- parse_stream("tmp.json")
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 0)
  expect_gt(ncol(x), 15)
  file.remove("tmp.json")
})
