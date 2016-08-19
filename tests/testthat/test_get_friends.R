skip_on_cran()

library(rtweet)

context("Get friends")

f <- get_friends("kearneymw")

test_that("get_friends returns tibble with ids", {
  expect_true(any(c("tbl_df", "tbl") %in% class(f)))
  expect_named(f, "ids")
  expect_gt(nrow(f), 200)
})
