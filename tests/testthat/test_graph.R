test_that("graphing functions work", {
  skip_on_cran()
  skip_if_offline()

  x <- search_tweets("twitter filter:verified", n = 200)
  d <- network_data(x)
  expect_true(
    is.data.frame(d)
  )
  expect_gt(nrow(d), 1)
  expect_equal(ncol(d), 3)
  g <- network_graph(x)
  expect_true(
    inherits(g, "igraph")
  )

})
