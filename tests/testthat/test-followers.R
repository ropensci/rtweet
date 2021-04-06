test_that("get_followers returns expected data", {
  users <- get_followers("KFC", n = 100)

  expect_s3_class(users, "tbl_df")
  expect_named(users, "user_id")
  expect_equal(nrow(users), 100)
  
  expect_type(next_cursor(users), "character")
})
