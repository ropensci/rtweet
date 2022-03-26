test_that("can favourite and unfavourite a tweet", {
  skip_if_offline()
  vcr::use_cassette("post_favorite", {
    tw <- suppressMessages(post_tweet(paste0("test favourite ", Sys.time())))
  })
  json <- httr::content(tw)
  
  vcr::use_cassette("post_favorite1", {
    expect_error(post_favorite(json$id_str), NA)
  })
  vcr::use_cassette("post_favorite2", {
    expect_error(post_favorite(json$id_str, destroy = TRUE), NA)
  })
  vcr::use_cassette("post_favorite3", {
    expect_error(post_destroy(json$id_str), NA)
  })
})
