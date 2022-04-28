test_that("mentions returns tweets data", {
  
  vcr::use_cassette("mentions", {
    suppressMessages(x <- get_mentions())
  })

  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)
})

vcr::use_cassette("mentions2", {
  test_that("mentions returns tweets format", {
    
    tw <- get_mentions()
    
    expect_s3_class(tw, "data.frame")
    expect_gt(nrow(tw), 0)
    expect_s3_class(users_data(tw), "data.frame")
    expect_true("id" %in% colnames(tw))
    
    expect_error(get_mentions(since_id = tw), NA)
  })
})
