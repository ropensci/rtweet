test_that("lat_lng works", {
  vcr::use_cassette("lat_lng", {
    rt <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 100)
  })
  ## create lat/lng variables using all available tweet and profile geo-location data
  pos <- lat_lng(rt)
  expect_false(any(is.na(pos$lat)))
  expect_true(is.numeric(pos$lat))
  expect_false(any(is.na(pos$lng)))
  expect_true(is.numeric(pos$lng))
  expect_gt(ncol(pos), ncol(rt))
})

test_that("lat_lng works on empty tweet", {
  expect_error(lat_lng(tweet(NULL)), NA)
  
})
