test_that("blocking and unblocking users", {
  pf <- post_block("s_hesz") 
  expect_equal(httr::status_code(pf), 200L)
  
  # unblocking
  pf <- post_unblock("s_hesz")
  expect_equal(httr::status_code(pf), 200L)
})
