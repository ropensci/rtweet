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

test_that("Stream for 10 seconds", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  new_rule <- stream_add_rule(list(value = "#rstats", tag = "rstats"))
  # Open filtered streaming connection for 20s
  tmp <- tempfile()
  expect_error({
    f <- filtered_stream(file = tmp, timeout = 10, parse = FALSE)
    f2 <- filtered_stream(file = tmp, timeout = 10, parse = FALSE)
  }, NA)
  # Remove rule
  stream_rm_rule(ids(new_rule))
  # Open random streaming connection
  expect_error(sample_stream(file = tempfile(), timeout = 10, parse = FALSE), NA)
})
