test_that("lookup_users works", {
  users <- c(
    "potus", "hillaryclinton", "realdonaldtrump",
    "fivethirtyeight", "cnn", "espn", "twitter"
  )
  usr_df <- lookup_users(users)
  expect_s3_class(usr_df, "tbl")
  expect_equal(nrow(usr_df), 6)
  expect_equal(ncol(usr_df), 91)
})
