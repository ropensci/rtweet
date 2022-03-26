test_that("lists_subscriptions returns lists data frame", {
  
  vcr::use_cassette("lists_subscriptions1", {
    x <- lists_subscriptions("kearneymw")
  })

  expect_true(is.data.frame(x))
  expect_true("list_id" %in% names(x))
  expect_true(nrow(x) >= 1)
})
