# Tests from https://gist.github.com/dieghernan/824425b814bbb9527db5085d6dacb157

skip("slow tests")

test_that("mp4 10MG works", {
  skip_if_offline()
  skip_on_cran()
  temp <- tempfile(fileext = ".mp4")
  url <-
    "https://file-examples-com.github.io/uploads/2017/04/file_example_MP4_1280_10MG.mp4"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  upload_media_to_twitter(temp)
})



test_that("mp4 5MG works", {
  skip_if_offline()
  skip_on_cran()
  ### Sample mp4 - (<5mb) ----
  temp <- tempfile(fileext = ".mp4")
  url <-
    "https://file-examples-com.github.io/uploads/2017/04/file_example_MP4_480_1_5MG.mp4"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  upload_media_to_twitter(temp)
})
## GIF----

### Sample animated gif (>5MB)----

test_that("GIF > 5MG works", {
  skip_if_offline()
  skip_on_cran()
  temp <- tempfile(fileext = ".gif")
  url <- "https://upload.wikimedia.org/wikipedia/commons/9/96/.Animation_of_Hayabusa2_orbit.gif"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  upload_media_to_twitter(temp)
})

### Sample animated gif (<5Mb)----
test_that("animated gif < 5MG works", {
  skip_if_offline()
  skip_on_cran()
  temp <- tempfile(fileext = ".gif")
  url <- "https://upload.wikimedia.org/wikipedia/commons/d/d0/01_Das_Sandberg-Modell.gif"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  upload_media_to_twitter(temp)
})


### Sample unanimated gif (<5Mb)----
test_that("unanimated gif < 5MG works", {
  skip_if_offline()
  skip_on_cran()
  temp <- tempfile(fileext = ".gif")
  url <- "https://file-examples-com.github.io/uploads/2017/10/file_example_PNG_500kB.png"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  upload_media_to_twitter(temp)
})

### Sample unanimated gif (>5Mb) SHOULD ERROR-----
test_that("unanimated gif >5MG works", {
  skip_if_offline()
  skip_on_cran()
  temp <- tempfile(fileext = ".gif")
  url <- "https://sample-videos.com/img/Sample-png-image-5mb.png"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  expect_error(upload_media_to_twitter(temp))
})

## PNG----
test_that("png works", {
  skip_if_offline()
  skip_on_cran()
  temp <- tempfile(fileext = ".png")
  url <- "https://file-examples-com.github.io/uploads/2017/10/file_example_PNG_500kB.png"
  download.file(url, temp, mode = "wb", quiet = TRUE)
  upload_media_to_twitter(temp)
})
