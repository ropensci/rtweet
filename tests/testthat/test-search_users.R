test_that("search_users returns users data", {
  skip_if_offline()
  vcr::use_cassette("search_users", {
    x <- search_users("twitter", n = 20, verbose = FALSE)
  })
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 20)
})
