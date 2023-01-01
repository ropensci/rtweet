test_that("list_statuses() works with either id or slug + owner_user", {

  vcr::use_cassette("lists_statuses1", {
    x <- lists_statuses("105140588", n = 5)
  })
  expect_s3_class(x, "data.frame")

  vcr::use_cassette("lists_statuses2", {
    x <- lists_statuses(slug = "senators", owner_user = "cspan", n = 5)
  })
  expect_s3_class(x, "data.frame")
})
