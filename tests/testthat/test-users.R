test_that("lookup_users returns users data", {
  x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 4)
})

test_that("lookup_users works", {
  users <- c(
    "potus", "hillaryclinton", "realdonaldtrump",
    "fivethirtyeight", "cnn", "espn", "twitter"
  )
  usr_df <- lookup_users(users)
  expect_s3_class(usr_df, "data.frame")
  expect_equal(nrow(usr_df), 6)
  expect_equal(ncol(usr_df), 21)
})
