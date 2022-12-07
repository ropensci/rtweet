# Needs a user account
test_that("blocking works", {
  skip("requires manual testing")
  # Use other account to block the user
  b <- user_block("rtweet_test")
  expect_equal(httr::status_code(b), 200L)
  a <- user_unblock("rtweet_test")
  expect_equal(httr::status_code(a), 200L)
})
