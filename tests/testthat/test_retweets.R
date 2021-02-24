test_that("get_retweets returns tweets data", {
  skip_on_cran()
  skip("requires kearneymw as twitter auth")
  
  x <- get_retweets("929511061954297857")
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("screen_name" %in% names(x))
})

test_that("get_retweeters returns users", {
  skip_on_cran()
  skip("requires kearneymw as twitter auth")
  
  x <- get_retweeters("929511061954297857")
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("user_id" %in% names(x))
})



