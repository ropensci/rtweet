test_that("tweet_shot", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()
  skip_if_not(webshot::is_phantomjs_installed())
  
  tw <- rtweet::tweet_shot("https://twitter.com/jhollist/status/947082036019388416")
  expect_equal(class(tw), "magick-image")
  expect_equal(length(tw), 1L)
})
