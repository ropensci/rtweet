test_that("get_followers returns expected data", {
  skip_if_offline()
  vcr::use_cassette("get_followers", {
    users <- get_followers("KFC")
  })
  
  # expect_s3_class(users, "data.frame")
  expect_named(users, c("from_id", "to_id"))
  expect_equal(nrow(users), 5000)
  
  expect_type(next_cursor(users), "character")
})
