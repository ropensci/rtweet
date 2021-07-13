test_that("blocking and unblocking users", {
  pf <- block_user("s_hesz") 
  expect_equal(httr::status_code(pf), 200L)
  
  # unblocking
  pf <- unblock_user("s_hesz")
  expect_equal(httr::status_code(pf), 200L)
})
