test_that("search_archive works", {
  skip_on_cran()
  sa <- expect_error(sa <- tweet_search_all("#rtweet", parse = FALSE, n = 10), NA)
  y <- auth_get()
  print(y$token)
  expect_length(sa$data, 10)
  expect_named(sa, c("data", "meta"))
})

test_that("search_recent works", {
  skip_on_cran()
  expect_error(sa <- tweet_search_recent("#rtweet", parse = FALSE, n = 10), NA)
  expect_length(sa$data, 10)
  expect_named(sa, c("data", "meta"))
})
