test_that("lists_subscriptions returns lists data frame", {
  x <- lists_subscriptions("kearneymw")

  expect_true(is.data.frame(x))
  expect_true("list_id" %in% names(x))
  expect_gt(nrow(x), 1)
})
