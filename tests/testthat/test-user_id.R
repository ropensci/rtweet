test_that("user_type handles simple cases", {
  expect_equal(user_type("hadleywickham"), "screen_name")
  
  expect_equal(user_type(123), "user_id")
  expect_equal(user_type(bit64::as.integer64(123)), "user_id")
  expect_equal(user_type("123"), "user_id")
  expect_equal(user_type(as_screenname("123")), "screen_name")
  
  expect_snapshot(user_type(TRUE), error = TRUE)
})
