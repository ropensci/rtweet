context("tweet_shot")

test_that("tweet_shot", {
  skip_on_cran()
  has_phantom <- function() {
    x <- suppressMessages(webshot:::find_phantom())
    length(x) == 1 && x != ""
  }
  if (requireNamespace("webshot", quietly = TRUE)) {
    tw <- rtweet::tweet_shot("https://twitter.com/jhollist/status/947082036019388416")
    expect_equal(class(tw), "magick-image")
    expect_equal(length(tw), 1L)
  }
})
