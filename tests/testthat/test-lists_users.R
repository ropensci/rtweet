test_that("lists_users returns data frame with nrow > 1", {
  
  vcr::use_cassette("list_users1", {
    x <- lists_users("kearneymw")
  })
  expect_true(is.data.frame(x))
  expect_gt(nrow(x), 0)
})

