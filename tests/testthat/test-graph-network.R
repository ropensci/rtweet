# Status (no retweet, no reply no quote)
test_that("network_data on status", {
  
  vcr::use_cassette("graph-network1", {
    status <- lookup_tweets("1333789433288540160")
    nd <- network_data(status, "mention")  
  })
  expect_s3_class(nd, "data.frame")
})

# Reply (no quote no retweet)
test_that("network_data on reply", {
  
  vcr::use_cassette("graph-network2", {
    reply <- lookup_tweets("1333789435482161153")
    nd <- network_data(reply, "reply")  
    })
  expect_s3_class(nd, "data.frame")
  })

# Retweet with other tweet embedded quoting  
test_that("network_data on retweet quoting", {
  
  vcr::use_cassette("graph-network3", {
    retweet_quoted <- lookup_tweets("1390610121743556609")
    nd <- network_data(retweet_quoted, "quote")  
  })
  expect_s3_class(nd, "data.frame")
})

# Retweet without adding anything new
test_that("network_data on retweet", {
  
  vcr::use_cassette("graph-network4", {
    retweet <- lookup_tweets("1390785143615467524")
    nd_retweet <- network_data(retweet, "retweet")
  })
  expect_s3_class(nd_retweet, "data.frame")
})


test_that("network_data on many", {
  
  vcr::use_cassette("graph-network5", {
    status <- lookup_tweets(c("1333789433288540160", "1333789435482161153", 
                              "1390610121743556609", "1390785143615467524"))
    nd <- network_data(status)  
  })
  expect_s3_class(nd, "data.frame")
})


test_that("graphing functions work", {
  
  vcr::use_cassette("graph-network6", {
    x <- search_tweets("twitter filter:verified", n = 200)
    d <- network_data(x)
  })
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
  
  vcr::use_cassette("graph-network7", {
    rstats <- search_tweets("#rstats", n = 20)
    ## create from-to data frame representing retweet/mention/reply connections
    rstats_net <- network_data(rstats, c("retweet","mention","reply"))
  })
  expect_s3_class(rstats_net, "data.frame")
  expect_equal(colnames(rstats_net), c("from", "to", "type"))
})

test_that("network_graph works", {
  
  vcr::use_cassette("graph-network8", {
    rstats <- search_tweets("#rstats", n = 20)
    
    ## create from-to data frame representing retweet/mention/reply connections
    rstats_net <- network_graph(rstats)
  })
  expect_s3_class(rstats_net, "igraph")
})

test_that("Network data works even with no retweet, #696", {
  
  vcr::use_cassette("graph-network9", {
    th10 <- tweet_threading("1256278649600225280", traverse = "backwards")
  })
  expect_error(nd <- network_data(th10), NA)
})
