test_that("lat_lng works", {
  expect_error(search_tweets("lang:en", geocode = lookup_coords("usa"), n = 100))
})

test_that("lat_lng works on empty tweet", {
  expect_error(lat_lng(tweet(NULL)), NA)

})
