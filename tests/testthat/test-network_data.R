test_that("network_data works", {
   rstats <- search_tweets("#rstats", n = 20)
   ## create from-to data frame representing retweet/mention/reply connections
   rstats_net <- network_data(rstats, "retweet,mention,reply")
   expect_s3_class(rstats_net, "data.frame")
  expect_equal(colnames(rstats_net), c("from", "to", "type"))
})


test_that("network_graph works", {
  rstats <- search_tweets("#rstats", n = 20)
  
  ## create from-to data frame representing retweet/mention/reply connections
  rstats_net <- network_graph(rstats)
  expect_s3_class(rstats_net, "igraph")
})
