test_that("graphing functions work", {
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
# https://twitter.com/henrikbengtsson/status/1390403676057980928
test_that("network_data works", {
   rstats <- search_tweets("#rstats", n = 20)
   ## create from-to data frame representing retweet/mention/reply connections
   rstats_net <- network_data(rstats, "retweet,mention,reply")
   expect_s3_class(rstats_net, "data.frame")
  expect_equal(colnames(rstats_net), c("from", "to", "type"))
})

lu <- lookup_tweets("1390403676057980928")
test_that("network_graph works", {
  rstats <- search_tweets("#rstats", n = 20)
  
  ## create from-to data frame representing retweet/mention/reply connections
  rstats_net <- network_graph(rstats)
  expect_s3_class(rstats_net, "igraph")
})
