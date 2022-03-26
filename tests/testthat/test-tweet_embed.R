test_that("tweet_embed() returns a string", {
  
  vcr::use_cassette("tweet_embed", {
    out <- tweet_embed("kearneymw", "1087047171306856451")
  })
  expect_type(out, "character")
})
