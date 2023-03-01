test_that("retweeted_by works", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  # Pinned tweets where deleted so there will be errors
  expect_error(tweet_retweeted_by("567053242429734913", parse = FALSE), NA)
  rb <- tweet_retweeted_by("567053242429734913", parse = TRUE)
  expect_s3_class(rb, "data.frame")

})
