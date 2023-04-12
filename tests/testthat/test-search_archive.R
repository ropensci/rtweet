test_that("search_archive works", {
  skip_on_cran()
  testing_with_authentication("bearer_academic_dev")
  sa <- expect_error(tweet_search_all("#rtweet", parse = FALSE, n = 10), NA)
  expect_gte(length(sa[[1]]$data), 1) # No garantee it returns at least 10 values
  expect_named(sa[[1]], c("data", "meta"))
})

test_that("search_recent works", {
  skip_on_cran()
  testing_with_authentication("bearer_testing_app")
  expect_error(sa <- tweet_search_recent("#rtweet", parse = FALSE, n = 10), NA)
  expect_gte(length(sa[[1]]$data), 1)
  expect_named(sa[[1]], c("data", "meta"))
})
