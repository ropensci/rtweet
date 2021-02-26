test_that("get_retweets returns tweets data", {
  skip_on_cran()
  
  x <- get_retweets("1363488961537130497")
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("screen_name" %in% names(x))
})

test_that("get_retweeters returns users", {
  skip_on_cran()

  x <- get_retweeters("1363488961537130497")
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
})
