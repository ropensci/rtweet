test_that("post_follow works", {
  pf <- post_follow("BarackObama")
  expect_equal(httr::status_code(pf), 200L)
})

test_that("post_destroy works", {
  pf <- post_follow("BarackObama", destroy = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})

test_that("post_follow works with notifications", {
  pf <- post_follow("BarackObama", notify = TRUE)
  expect_equal(httr::status_code(pf), 200L)
  pf <- post_follow("BarackObama", destroy = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})


test_that("Muting  #467", {
  pf <- post_follow("hlynur", destroy = TRUE, mute = TRUE, notify = TRUE) 
  expect_equal(httr::status_code(pf), 200L)
  # Unmutting without following
  pf <- post_follow("hlynur", destroy = TRUE, mute = FALSE, notify = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})
