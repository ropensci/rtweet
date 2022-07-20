test_that("lists_subscribers returns users data frame", {
  
  vcr::use_cassette("lists_subscribers", {
    x <- lists_subscribers(
      slug = "new-york-times-politics",
      owner_user = "nytpolitics",
      n = 20
    )
  })
  expect_true(is.data.frame(x))
  expect_true("description" %in% names(x))
  expect_gt(nrow(x), 10)
})
