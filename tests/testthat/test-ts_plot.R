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
  skip_if_not_installed("dplyr")
  vcr::use_cassette("ts_plot3", {
    rt <- search_tweets("rstats", n = 100)
  })
  expect_error(ts_plot(dplyr::group_by(rt, is_quote_status)), NA)
})
