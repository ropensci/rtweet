test_that("stream_tweets returns tweets data", {
  skip_if_offline()
  # Randomly skip test to avoid unofficial 420 calm down response from twitter
  if (sample(c(TRUE, FALSE, FALSE, FALSE), 1)) {
    skip_on_ci()
  }
  path <- tempfile()
  x1 <- stream_tweets(timeout = 1, file_name = path, verbose = FALSE)
  expect_s3_class(x1, "data.frame")
  expect_error(ud <- users_data(x1), NA)

  stream_tweets(timeout = 1, file_name = path, verbose = FALSE, parse = FALSE)
  
  x2 <- parse_stream(path)
  expect_true(nrow(x2) > nrow(x1))
})
