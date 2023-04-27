test_that("req_error works", {
  testing_with_authentication("renewed_token")
  expect_error(user_blocked("407200271", n = Inf),
               "must be the same as the authenticating user")
})
