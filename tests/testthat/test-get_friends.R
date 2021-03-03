test_that("get_friends works", {
  djt <- get_friends("ropensci")
  expect_s3_class(djt, "tbl")
})
