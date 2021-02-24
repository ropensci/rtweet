context("test-access-level")

test_that("api_access_level works", {
  skip_on_cran()
  skip_if_offline()
  
  a <- api_access_level()
  expect_true(is.character(a) && length(a) == 1)
})
