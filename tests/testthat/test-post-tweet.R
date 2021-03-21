test_that("upload_media_to_twitter() can handle small file", {
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
  id <- upload_media_to_twitter(test_path("tweet.gif"), alt_text = "A bird tweeting")
  expect_type(id, "character")
})


test_that("post_tweet works", {
  msg <- paste("test", Sys.time()) # To avoid having duplicated status
  rt <- expect_message(post_tweet(msg), "your tweet has been posted!")
  crt <- httr::content(rt)
  rt <- expect_message(post_destroy(crt$id_str), "your tweet has been deleted!")
  expect_equal(httr::status_code(rt), 200L)
})

test_that("post_tweet geolocated works", {
  # Test errors
  msg <- paste("test geolocated error", Sys.time()) # To avoid having duplicated status
  expect_error(post_tweet(msg, lat = "x", long = 0))
  expect_error(post_tweet(msg, lat = 0, long = "x"))
  expect_error(post_tweet(msg, lat = 91, long = 0))
  expect_error(post_tweet(msg, lat = 0, long = 181))
  expect_error(post_tweet(msg, lat = 0, long = 0, display_coordinates = "error"))
  
  # Test geolocated tweet
  msg <- paste("test geolocated", Sys.time()) # To avoid having duplicated status
  rt <- expect_message(post_tweet(msg, lat = -36.811784, long = 174.792657), "your tweet has been posted!")
  crt <- httr::content(rt)
  rt <- expect_message(post_destroy(crt$id_str), "your tweet has been deleted!")
  expect_equal(httr::status_code(rt), 200L)

  # Test display_coordinates param
  msg <- paste("test geolocated", Sys.time()) # To avoid having duplicated status
  rt <- expect_message(post_tweet(msg, lat = -36.811784, long = 174.792657, display_coordinates = TRUE), "your tweet has been posted!")
  crt <- httr::content(rt)
  rt <- expect_message(post_destroy(crt$id_str), "your tweet has been deleted!")
  expect_equal(httr::status_code(rt), 200L)
})
