test_that("get_followers works", {
  kfc <- get_followers("KFC")
  expect_s3_class(kfc, "tbl")
  expect_equal(nrow(kfc), 5000)
})
