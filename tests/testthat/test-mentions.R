test_that("mentions returns tweets data", {
  skip_if_offline()
  vcr::use_cassette("mentions", {
    suppressMessages(x <- get_mentions())
  })

  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)
})
