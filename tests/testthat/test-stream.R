test_that("stream_tweets returns tweets data", {
  path <- tempfile()
  x1 <- stream_tweets(timeout = 1, file_name = path, verbose = FALSE)
  expect_s3_class(x1, "data.frame")
  
  x2 <- stream_tweets(timeout = 1, file_name = path, verbose = FALSE)
  expect_true(nrow(x2) > nrow(x1))
})
