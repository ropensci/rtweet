test_that("destroytoken", {
  if (file.exists("twitter_tokens")) {
    unlink("twitter_tokens")
  }
  expect_true(TRUE)
})
