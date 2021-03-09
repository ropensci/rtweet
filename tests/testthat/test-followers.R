test_that("get_followers returns data frame with user_id", {
  f <- get_followers("HillaryClinton", n = 100)

  expect_s3_class(f, "tbl_df")
  expect_named(f, "user_id")
  expect_gt(NROW(f), 99)
})

test_that("get_followers works", {
  kfc <- get_followers("KFC")
  expect_s3_class(kfc, "tbl")
  expect_equal(nrow(kfc), 5000)
})
