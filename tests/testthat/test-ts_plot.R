test_that("ts_plot works", {
  rt <- search_tweets("rstats", n = 100)
  expect_error(ts_plot(rt), NA)
})


test_that("ts_data works", {
  rt <- search_tweets("rstats", n = 100)
  expect_error(ts_data(rt), NA)
})
