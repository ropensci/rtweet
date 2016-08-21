context("get_followers")

test_that("get_followers returns tibble with ids", {
	skip_on_cran()
	skip_on_travis()

	f <- get_followers("HillaryClinton", n = 10000)

  expect_true(any(c("tbl_df", "tbl") %in% class(f)))
  expect_named(f, "ids")
  expect_equal(nrow(f), 10000)
})
