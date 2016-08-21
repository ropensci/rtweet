library(rtweet)

context("get_friends")

skip_on_cran()
skip_on_travis()
f <- get_friends("kearneymw")

test_that("get_friends returns tibble with ids", {
	skip_on_cran()
	skip_on_travis()
  expect_true(any(c("tbl_df", "tbl") %in% class(f)))
  expect_named(f, "ids")
  expect_gt(nrow(f), 200)
})
