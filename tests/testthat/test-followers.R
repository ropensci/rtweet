test_that("get_followers returns expected data", {
  users <- get_followers("KFC")

  expect_s3_class(users, "data.frame")
  expect_named(users, "user_id")
  expect_equal(nrow(users), 5000)
  
  expect_type(next_cursor(users), "character")
})
