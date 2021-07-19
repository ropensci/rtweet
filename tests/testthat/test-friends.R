test_that("get_friends returns data frame with ids", {
  f <- get_friends("kearneymw")

  expect_true(is.data.frame(f))
  ##expect_true(is.character(f[["ids"]]))
  expect_gt(nrow(f), 200)
})

test_that("friendships returns data", {
  x <- my_friendships("kearneymw")
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("screen_name" %in% names(x))

  x <- lookup_friendships("kearneymw", c("realdonaldtrump", "cstonehoops"))

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("relationship" %in% names(x))
})

test_that("get_friends works", {
  djt <- get_friends("ropensci")
  expect_s3_class(djt, "data.frame")
})

test_that("lookup_friendships works", {
  lf <- lookup_friendships("hadley", "Lluis_Revilla")
  expect_s3_class(lf, "data.frame")
})

test_that("my_friendships works", {
  mf <- my_friendships("hadley")
  expect_s3_class(mf, "data.frame")
})
