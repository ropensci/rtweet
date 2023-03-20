# Needs a user account
test_that("blocking works", {
  skip("requires manual testing")
  # Use other account to block the user
  b <- user_block("Lluis_Revilla")
  expect_equal(httr::status_code(b), 200L)
  a <- user_unblock("Lluis_Revilla")
  expect_equal(httr::status_code(a), 200L)
})
