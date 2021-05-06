test_that("get_timeline works", {
  x <- get_timeline(c("cnnbrk", "cnn"), n = 400)
  expect_s3_class(x, "tbl_df")
  expect_true("id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 20)
})

test_that("get_my_timeline() works", {
  gmt <- get_my_timeline()
  expect_s3_class(gmt, "tbl_df")
  expect_true(nrow(gmt) > 50)
})

test_that("get_timelines() is deprecated", {
  expect_snapshot(x <- get_timelines("cnn", n = 10))
})

