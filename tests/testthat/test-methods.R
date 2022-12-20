test_that("methods work", {
  vcr::use_cassette("methods1", {
    R_foundation_favs <- get_favorites("_R_Foundation", n = 10)
  })

  expect_error(urls(head(R_foundation_favs)), NA)
  expect_error(hashtags(head(R_foundation_favs)), NA)
  expect_error(symbols(head(R_foundation_favs)), NA)
  expect_error(user_mentions(head(R_foundation_favs)), NA)
  expect_error(media(head(R_foundation_favs)), NA)
})
