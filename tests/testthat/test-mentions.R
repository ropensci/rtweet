test_that("mentions returns tweets data", {
  suppressMessages(x <- get_mentions())

  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)
})
