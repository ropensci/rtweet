test_that("stream_tweets returns tweets data", {
  skip_on_cran()
  skip_if_offline()

  path <- tempfile()
  x1 <- stream_tweets(timeout = 1, file_name = path, verbose = FALSE)
  expect_s3_class(x1, "tbl_df")
  
  x2 <- stream_tweets(timeout = 1, file_name = path, verbose = FALSE)
  expect_true(nrow(x2) > nrow(x1))
})
