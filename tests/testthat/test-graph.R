test_that("graphing functions work", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  x <- search_tweets("twitter filter:verified", n = 200, token = token)
  expect_true(
    {d <- network_data(x)
    is.data.frame(d)}
  )
  expect_gt(nrow(d), 1)
  expect_equal(ncol(d), 3)

  expect_true(
    {g <- network_graph(x)
    inherits(g, "igraph")}
  )

})

