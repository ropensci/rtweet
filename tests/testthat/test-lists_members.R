test_that("list_members() works with either id or slug + owner_user", {
  
  vcr::use_cassette("lists_members1", {
    x <- lists_members("105140588")
  })
  expect_s3_class(x, "data.frame")
  
  vcr::use_cassette("lists_members2", {
    x <- lists_members(slug = "senators", owner_user = "cspan")
  })
  expect_s3_class(x, "data.frame")
})
