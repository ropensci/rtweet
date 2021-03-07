test_that("post_follow works", {
  pf <- post_follow("BarackObama")
  expect_equal(httr::status_code(pf), 200L)
})

test_that("post_destroy works", {
  pf <- post_follow("BarackObama", destroy = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})

test_that("post_follow works without retweets", {
  # First we need to follow then we can disable retweets
  pf <- post_follow("BarackObama") 
  pf <- post_follow("BarackObama", retweets = FALSE)
  expect_equal(httr::status_code(pf), 200L)
  pf <- post_follow("BarackObama", destroy = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})

test_that("post_follow works without notifications", {
  pf <- post_follow("BarackObama", notify = FALSE)
  expect_equal(httr::status_code(pf), 200L)
  pf <- post_follow("BarackObama", destroy = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})
