test_that("stream_tweets returns tweets data", {
  skip(message = "No longer working")
  path <- tempfile()
  x1 <- stream_tweets(timeout = 1, file_name = path, verbose = FALSE)
  expect_s3_class(x1, "tweets")
  expect_error(ud <- users_data(x1), NA)

  stream_tweets(timeout = 1, file_name = path, verbose = FALSE, parse = FALSE)

  x2 <- parse_stream(path)
  expect_true(nrow(x2) > nrow(x1))
})
