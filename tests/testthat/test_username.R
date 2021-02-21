context("test-test_username-r")

test_that("test authenticating user name", {
  skip_on_cran()
  sn <- authenticating_user_name()
  expect_type(sn, "character")
  expect_length(sn, 1)
})
