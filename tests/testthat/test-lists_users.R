test_that("lists_users returns data frame with nrow > 1", {
  skip_on_cran()
  skip_if_offline()

  x <- lists_users("kearneymw")
  expect_true(is.data.frame(x))
  expect_gt(nrow(x), 0)
})

