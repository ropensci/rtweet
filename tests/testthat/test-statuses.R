test_that("lookup_statuses returns users data", {
  ids <- c("558115838503690243", "760182486005583872", "776053079540166657")
  x <- lookup_tweets(ids)
  
  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 2) # 558115838503690243 was deleted
})
