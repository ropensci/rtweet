test_that("tweet_shot", {
  skip_on_ci()
  skip_if_not(webshot::is_phantomjs_installed())
  
  tw <- rtweet::tweet_shot("https://twitter.com/jhollist/status/947082036019388416")
  expect_equal(class(tw), "magick-image")
  expect_equal(length(tw), 1L)
  tw2 <- rtweet::tweet_shot("947082036019388416")
  expect_equal(class(tw2), "magick-image")
  expect_equal(length(tw2), 1L)
  expect_true(all.equal(tw, tw2))
})

test_that("tweet_shot correct image", {
  skip_on_ci()
  skip_if_not(webshot::is_phantomjs_installed())
  skip("requires visual check")
  
  tw <- rtweet::tweet_shot("https://twitter.com/jhollist/status/947082036019388416")
})
