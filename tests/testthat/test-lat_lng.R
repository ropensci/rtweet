test_that("lat_lng works", {
  vcr::use_cassette("lat_lng", {
    rt <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 100)
  })
  ## create lat/lng variables using all available tweet and profile geo-location data
  rt <- lat_lng(rt)
  expect_false(any(!is.na(rt$lang)))
})
