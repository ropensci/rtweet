test_that("ts_plot works", {

  vcr::use_cassette("ts_plot1", {
    rt <- search_tweets("rstats", n = 100)
  })
  expect_error(ts_plot(rt), NA)
})


test_that("ts_data works", {

  vcr::use_cassette("ts_plot2", {
    rt <- search_tweets("rstats", n = 100)
  })
  expect_error(ts_data(rt), NA)
})

test_that("ts_plot grouped works", {

  vcr::use_cassette("ts_plot3", {
    rt <- search_tweets("rstats", n = 100)
  })
  expect_error(ts_plot(dplyr::group_by(rt, screen_name)), NA)
})

test_that("ts_data grouped works", {

  vcr::use_cassette("ts_plot4", {
    rt <- search_tweets("rstats", n = 100)
  })
  expect_error(ts_data(dplyr::group_by(rt, screen_name)), NA)
})

