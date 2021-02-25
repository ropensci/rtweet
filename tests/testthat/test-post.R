test_that("upload_media_to_twitter() can handle small file", {
  skip_on_cran()
  
  id <- upload_media_to_twitter(test_path("tweet.gif"))
  expect_equal(wait_for_chunked_media(id), "succeeded")
  
  id <- upload_media_to_twitter(test_path("tweet.gif"), chunk_size = 1024)
  expect_equal(wait_for_chunked_media(id), "succeeded")
})

test_that("upload_media_to_twitter() can handle chunked upload", {
  # Download and cache ~1.5 MB mpg
  large_mp4 <- test_path("large.mp4")
  if (!file.exists(large_mp4)) {
    download.file("https://file-examples-com.github.io/uploads/2017/04/file_example_MP4_480_1_5MG.mp4", large_mp4, quiet = TRUE)
  }
  
  id <- upload_media_to_twitter(large_mp4, chunk_size = 1024 * 1024)
  expect_equal(wait_for_chunked_media(id), "succeeded")
})
