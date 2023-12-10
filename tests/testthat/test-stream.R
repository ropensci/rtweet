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
