test_that("get_trends returns trends data", {

  vcr::use_cassette("get_trends1", {
    x <- get_trends()
  })

  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true(all(c("trend", "promoted_content") %in% names(x)))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 5)
  expect_equal(check_woeid("world"), "1")
  expect_equal(check_woeid("kansas"), "2347575")
  expect_equal(check_woeid("new york"), "2347591")
})

test_that("trends_available returns data frame", {

  vcr::use_cassette("get_trends2", {
    x <- trends_available()
  })

  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true(all(c("name", "woeid") %in% names(x)))
  expect_gt(nrow(x), 5)
  expect_gt(ncol(x), 2)
  expect_equal(as.integer(subset(x, name == "Worldwide", select = "woeid")), 1)
})

test_that("get_trends_closest returns data frame with correct city name", {

  vcr::use_cassette("get_trends3", {
    x <- get_trends(lat = 40.7, lng = -74.0)
  })

  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true(all(c("trend", "woeid") %in% names(x)))
  expect_gt(nrow(x), 5)
  expect_gt(ncol(x), 2)
  expect_equal(as.character(x[1,]$place), "New York")
})
