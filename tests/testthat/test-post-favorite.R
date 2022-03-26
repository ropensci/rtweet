vcr::use_cassette("post_favorite", {
  test_that("can favourite and unfavourite a tweet", {
    
    tw <- suppressMessages(post_tweet(paste0("test favourite ", Sys.time())))
    json <- httr::content(tw)
    
    expect_error(post_favorite(json$id_str), NA)
    expect_error(post_favorite(json$id_str, destroy = TRUE), NA)
    expect_message(expect_error(post_destroy(json$id_str), NA), 
                   "Your tweet has been deleted!")
  })
})
