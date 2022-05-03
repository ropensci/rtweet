vcr::use_cassette("block", {
  test_that("blocking works", {
    b <- user_block("rtweet_test")
    expect_equal(httr::status_code(b), 200L)
    a <- user_unblock("rtweet_test")
    expect_equal(httr::status_code(a), 200L)
  })
})
