test_that("blocking and unblocking users", {
  auth_as()
  pf <- user_block("s_hesz")
  expect_equal(httr::status_code(pf), 200L)

  # unblocking
  pf <- user_unblock("s_hesz")
  expect_equal(httr::status_code(pf), 200L)
})
