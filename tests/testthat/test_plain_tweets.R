context("plain_tweets")

test_that("plain_tweets functions", {
  skip_on_cran()

  text <- c(" my website is http://mikewk.com or \njust enter mikewk.com into
  the nav     bar  ")

  expect_true(is.data.frame(plain_tweets(data.frame(text))))
  expect_true(is.list(plain_tweets(list(text = text))))
  expect_true(is.character(plain_tweets(text)))
  expect_true(grepl("^\\S", plain_tweets(text)))
  expect_true(!grepl("http", plain_tweets(text)))
  expect_true(!grepl("\\n", plain_tweets(text)))
})
