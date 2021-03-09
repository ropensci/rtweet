test_that("api_access_level works", {
  a <- api_access_level()
  expect_true(is.character(a) && length(a) == 1)
})
