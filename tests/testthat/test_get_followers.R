skip_on_cran()

library(rtweet)

context("Get followers")

f <- get_followers("HillaryClinton", n = 10000)

test_that("get_followers returns tibble with ids", {
  expect_true(any(c("tbl_df", "tbl") %in% class(f)))
  expect_named(f, "ids")
  expect_equal(nrow(f), 10000)
})
