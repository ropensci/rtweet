test_that("users_data works", {
  tweets <- search_tweets("r")
  users <- users_data(tweets)
  expect_s3_class(users, "tbl")
})
