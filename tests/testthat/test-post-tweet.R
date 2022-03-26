test_that("upload_media_to_twitter() can handle small file", {
  
  # gif - unchunk 1MB
  vcr::use_cassette("upload_media_to_twitter1", {
    id <- upload_media_to_twitter(test_path("tweet.gif"), chunk_size = 1024 * 1024)
  })
  expect_type(id, "character")
  
  # doesn't appear to be any way to get info about a media object
  # after it's been uploaded
  
  # gif - chunk 5KB
  vcr::use_cassette("upload_media_to_twitter2", {
    id <- upload_media_to_twitter(test_path("tweet.gif"), chunk_size = 5 * 1024)
  })
  expect_type(id, "character")
  
  # mp4 - test forced chunk / small file w/ high chunk sizes
  vcr::use_cassette("upload_media_to_twitter3", {
    id <- upload_media_to_twitter(test_path("tweet.mp4"), chunk_size = 1024 * 1024)
  })
  expect_type(id, "character")
})


test_that("can set alt text", {
  
  vcr::use_cassette("upload_media_to_twitter4", {
    id <- upload_media_to_twitter(test_path("tweet.gif"), alt_text = "A bird tweeting")
  })
  expect_type(id, "character")
})


test_that("post_tweet works", {
  skip_if_offline() # destroy id changes on each test
  msg <- paste("test", Sys.time()) # To avoid having duplicated status
  
  expect_message(pt <- post_tweet(msg), "Your tweet has been posted!")
  #pt = post tweet
  cpt <- httr::content(pt) 
  expect_equal(httr::status_code(pt), 200L)
  expect_message(dt <- post_destroy(cpt$id_str), "Your tweet has been deleted!")
  # dt = destroy tweet
  expect_equal(httr::status_code(dt), 200L)
})

test_that("post_tweet geolocated works", {
  skip_if_offline() # destroy id changes on each test
  
  # Test geolocated tweet
  msg <- paste("test geolocated", Sys.time()) # To avoid having duplicated status
  expect_message(pt <- post_tweet(msg, lat = -36.811784, long = 174.792657), 
                 "Your tweet has been posted!")
  cpt <- httr::content(pt)
  expect_message(dt <- post_destroy(cpt$id_str), "Your tweet has been deleted!")
  expect_equal(httr::status_code(dt), 200L)
  
  # Test display_coordinates param
  msg <- paste("test geolocated", Sys.time()) # To avoid having duplicated status
  expect_message(pt <- post_tweet(msg, lat = -36.811784, long = 174.792657,
                                  display_coordinates = TRUE), 
                 "Your tweet has been posted!")
  cpt <- httr::content(pt)
  
  expect_message(dt <- post_destroy(cpt$id_str), "Your tweet has been deleted!")
  expect_equal(httr::status_code(dt), 200L)
})

test_that("Check geo-related inputs for post_tweet", {
  # All these post_tweets fail
  expect_snapshot(error = TRUE, {
    msg <- paste("test geolocated error", Sys.time()) # To avoid having duplicated status
    post_tweet(msg, lat = "x", long = 0) 
    post_tweet(msg, lat = 0, long = "x")
    post_tweet(msg, lat = 91, long = 0)
    post_tweet(msg, lat = 0, long = 181)
    post_tweet(msg, lat = 0, long = 0, display_coordinates = "error")
  })
})
