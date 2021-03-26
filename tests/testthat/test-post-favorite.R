test_that("can favourite and unfavourite a tweet", {
  tw <- suppressMessages(post_tweet(paste0("test favourite ", Sys.time())))
  json <- httr::content(tw)

  expect_error(post_favorite(json$id_str), NA)
  expect_error(post_favorite(json$id_str, destroy = TRUE), NA)
})
