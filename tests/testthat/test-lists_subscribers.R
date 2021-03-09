test_that("lists_subscribers returns users data frame", {
  x <- lists_subscribers(
    slug = "new-york-times-politics",
    owner_user = "nytpolitics",
    n = 200
  )

  expect_true(is.data.frame(x))
  expect_true("description" %in% names(x))
  expect_gt(nrow(x), 50)
})
