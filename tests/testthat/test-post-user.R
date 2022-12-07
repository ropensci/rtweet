test_that("can follow and unfollow", {
  vcr::use_cassette("post_follow1", {
    pf <- post_follow("BarackObama")
  })
  expect_equal(httr::status_code(pf), 200L)

  vcr::use_cassette("post_follow2", {
    pf <- post_follow("BarackObama", destroy = TRUE)
  })
  expect_equal(httr::status_code(pf), 200L)
})

test_that("can follow and unfollow with notifications", {
  vcr::use_cassette("post_follow3", {
    pf <- post_follow("BarackObama", notify = TRUE)
  })
  expect_equal(httr::status_code(pf), 200L)

  vcr::use_cassette("post_follow4", {
    pf <- post_follow("BarackObama", destroy = TRUE)
  })
  expect_equal(httr::status_code(pf), 200L)
})

test_that("Muting  #467", {

  vcr::use_cassette("post_follow5", {
    pf <- post_follow("hlynur", destroy = TRUE, mute = TRUE, notify = TRUE)
  })
  expect_equal(httr::status_code(pf), 200L)
  # Unmutting without following
  vcr::use_cassette("post_follow6", {
    pf <- post_follow("hlynur", destroy = TRUE, mute = FALSE, notify = TRUE)
  })
  expect_equal(httr::status_code(pf), 200L)
})
