test_that("search_archive works", {
  testing_with_authentication()
  expect_error(sa <- search_archive("#rtweet", parse = FALSE, max_results = 10), NA)
  expect_length(sa$data, 10)
  expect_named(sa, c("data", "meta"))
})

test_that("search_recent works", {
  testing_with_authentication()
  expect_error(sa <- search_recent("#rtweet", parse = FALSE, max_results = 10), NA)
  expect_length(sa$data, 10)
  expect_named(sa, c("data", "meta"))
})
