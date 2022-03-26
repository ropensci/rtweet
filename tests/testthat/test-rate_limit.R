test_that("rate_limit works", {
  
  vcr::use_cassette("rate_limit1", {
    rl <- rate_limit()
  })
  expect_s3_class(rl, "data.frame")
})

test_that("rate_limit works", {
  
  vcr::use_cassette("rate_limit2", {
    rl <- rate_limit("application/rate_limit_status")
  })
  expect_s3_class(rl, "data.frame")
  expect_equal(nrow(rl), 1)
})

test_that("rate_limit returns rate_limit data", {
  
  vcr::use_cassette("rate_limit3", {
    x <- rate_limit()
  })
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("remaining" %in% names(x))
  expect_gt(nrow(x), 2)
  expect_gt(ncol(x), 2)
})

test_that("rate_limit_reset works", {
  
  vcr::use_cassette("rate_limit4", {
    reset <- rate_limit_reset("application/rate_limit_status")
  })
  expect_s3_class(reset, "POSIXct")
})

test_that("rate_limit_wait works", {
  
  vcr::use_cassette("rate_limit5", {
    wait <- rate_limit_wait("application/rate_limit_status")
  })
  expect_null(wait)
})

