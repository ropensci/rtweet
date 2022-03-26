test_that("lists_memberships returns data frame with nrow > 1", {
  
  vcr::use_cassette("lists_memberships", {
    df <- lists_memberships("kearneymw", filter_to_owned_lists = TRUE)
  })
  expect_s3_class(df, "data.frame")
  expect_equal(df$name, "test-memberships")
})
