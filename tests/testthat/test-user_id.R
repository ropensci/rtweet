test_that("screen_name has print and [ methods", {
  x <- as_screenname("123456")
  expect_s3_class(x[1], "rtweet_screen_name")
  expect_snapshot(x)
})

test_that("user_type handles simple cases", {
  expect_equal(user_type("hadleywickham"), "screen_name")

  expect_equal(user_type(123), "user_id")
  expect_equal(user_type(bit64::as.integer64(123)), "user_id")
  expect_equal(user_type("123"), "user_id")
  expect_equal(user_type(as_screenname("123")), "screen_name")

  expect_error(user_type(TRUE),
               "`user` must be a screen name or user id")
})
