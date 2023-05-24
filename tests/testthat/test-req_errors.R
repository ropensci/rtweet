test_that("req_error works", {
  testing_with_authentication("bearer_testing_app")
  expect_error(user_blocked("407200271", n = Inf))
})
