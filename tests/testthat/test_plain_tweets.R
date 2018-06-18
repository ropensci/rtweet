context("plain_tweets")

test_that("plain_tweets functions", {
  skip_on_cran()

  txt <- c(" my website is http://mikewk.com or \njust enter mikewk.com into
  the nav     bar  ")
  x <- data.frame(text = txt, stringsAsFactors = FALSE)
  x <- plain_tweets(x)
  expect_true(is.data.frame(x))
  x <- list(text = txt)
  x <- plain_tweets(x)
  expect_true(is.list(x))
  x <- plain_tweets(txt)
  expect_true(is.character(x))
  expect_true(grepl("^\\S", x))
  expect_true(!grepl("http", x))
  expect_true(!grepl("\\n", x))
})
