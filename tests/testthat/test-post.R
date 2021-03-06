test_that("upload_media_to_twitter() can handle small file", {
  skip_on_cran()

  # gif - unchunk 1MB
  id <- upload_media_to_twitter(test_path("tweet.gif"), chunk_size = 1024 * 1024)
  expect_type(id, "character")

  # doesn't appear to be any way to get info about a media object
  # after it's been uploaded

  # gif - chunk 5KB
  id <- upload_media_to_twitter(test_path("tweet.gif"), chunk_size = 5 * 1024)
  expect_type(id, "character")

  # mp4 - test forced chunk / small file w/ high chunk sizes
  id <- upload_media_to_twitter(test_path("tweet.mp4"), chunk_size = 1024 * 1024)
  expect_type(id, "character")
})


test_that("can set alt text", {
  skip_on_cran()
  
  id <- upload_media_to_twitter(test_path("tweet.gif"), alt_text = "A bird tweeting")
  expect_type(id, "character")
})


test_that("post_tweet works", {
  skip_if_offline()
  skip_on_cran()
  msg <- paste("test", Sys.time()) # To avoid having duplicated status
  rt <- expect_message(post_tweet(msg), "your tweet has been posted!")
  crt <- httr::content(rt)
  rt <- expect_message(post_destroy(crt$id_str), "your tweet has been deleted!")
  expect_equal(httr::status_code(rt), 200L)
})
