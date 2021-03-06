test_that("post_follow works", {
  pf <- post_follow("BarackObama")
  expect_equal(httr::status_code(pf), 200L)
})
test_that("post_destroy works", {
  pf <- post_follow("BarackObama", destroy = TRUE)
  expect_equal(httr::status_code(pf), 200L)
})
