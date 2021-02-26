test_that("get_friends returns data frame with ids", {
  skip_on_cran()
  skip_if_offline()

  f <- get_friends("kearneymw")
  expect_true(is.data.frame(f))
  expect_gt(nrow(f), 200)
})

test_that("friendships returns data", {
  skip_on_cran()
  skip_if_offline()

  x <- my_friendships("kearneymw")
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("screen_name" %in% names(x))

  x <- lookup_friendships("kearneymw", c("realdonaldtrump", "cstonehoops"))

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("relationship" %in% names(x))
})

