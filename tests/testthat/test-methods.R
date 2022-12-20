test_that("methods work", {
  vcr::use_cassette("methods1", {
    R_foundation_favs <- get_favorites("_R_Foundation", n = 10)
  })

  expect_error(entity(R_foundation_favs, "urls"), NA)
  expect_error(entity(R_foundation_favs, "hashtags"), NA)
  expect_error(entity(R_foundation_favs, "symbols"), NA)
  expect_error(entity(R_foundation_favs, "user_mentions"), NA)
  expect_error(entity(R_foundation_favs, "media"), NA)
})
