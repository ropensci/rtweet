test_that("tweet works with withheld_* #647", {
  
  vcr::use_cassette("lookup_tweets", {
    lu <- rtweet::lookup_tweets("1168343686863892480")
  })
  expect_true(is.data.frame(lu))
  expect_true(lu$withheld_copyright)
  expect_equal(lu$withheld_in_countries, list("XY"))
  expect_equal(lu$withheld_scope, "status")
})
