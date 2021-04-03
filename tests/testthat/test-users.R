test_that("lookup_users returns users data", {
  x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))

  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 4)
})

test_that("lookup_users works", {
  users <- c(
    "potus", "hillaryclinton", "realdonaldtrump",
    "fivethirtyeight", "cnn", "espn", "twitter"
  )
  usr_df <- lookup_users(users)
  expect_s3_class(usr_df, "tbl")
  expect_equal(nrow(usr_df), 6)
  expect_equal(ncol(usr_df), 20)
})
