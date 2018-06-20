context("get_my_timeline")

test_that("get_my_timeline", {
  skip_on_cran()

  expect_equal(rtweet:::home_user(), "kearneymw")
  token <- readRDS("twitter_tokens")
  x <- get_my_timeline()
  expect_true(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 50)
  expect_gt(ncol(x), 25)

  expect_equal(class(as_userid("101342234")), "user_id")
  expect_equal(class(as_screenname("101342234")), "screen_name")
  #x <- print(as_screenname("asdf"))
  #expect_equal(x, "asdf")
})
