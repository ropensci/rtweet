library(graphTweets)

context("test overhaul")

test_that("errors", {
  
  tweets <- data.frame(text = c("I tweet @you about @him",
                                "I tweet @me about @you"),
                       screen_name = c("me", "him"),
                       hashtags = c("rstats", "python"),
                       stringsAsFactors = FALSE)
  
  expect_error(gt_edges(tweets))
  
  expect_error(gt_nodes())
  expect_error(gt_nodes(tweets))
  
  expect_error(gt_edges())
  
  lst <- list(tweets)
  expect_error(gt_edges(lst))
  expect_error(gt_edges_(lst))
  expect_error(gt_edges_())
  expect_error(gt_co_edges())
  
  expect_error(
    gt <- tweets %>% 
      gt_edges_() %>% 
      gt_nodes() %>% 
      gt_dyn()
  )
})

test_that("nodes & edges & dyn", {
  
  tweets <- data.frame(
    text = c("I tweet @you about @him", 
             "I tweet @me about @you"),
    hashtags = c("rstats", "python"),
    screen_name = c("me", "him"),
    retweet_count = c(19, 5),
    status_id = c(1, 2),
    created_at = c(Sys.time(), Sys.time() + 15000),
    stringsAsFactors = FALSE
  )
  
  expect_is(gt_edges_(tweets), "graphTweets")
  expect_is(gt_edges(tweets, screen_name, text, status_id), "graphTweets")
  expect_is(gt_edges_(tweets, RT = "retweet_count"), "graphTweets")
  expect_is(gt_edges(tweets, screen_name, text, status_id, "retweet_count"), "graphTweets")
  edges <- gt_edges_(tweets)
  
  expect_is(gt_nodes(edges), "graphTweets")
  
  expect_is(tweets %>% gt_edges_() %>% gt_collect(), "list")
  expect_is(tweets %>% gt_edges_() %>% gt_nodes %>% gt_collect(), "list")
  expect_is(tweets %>% gt_edges_() %>% gt_nodes %>% gt_graph(), "igraph")
  expect_is(tweets %>% gt_edges_() %>% gt_graph(), "igraph")
  tweets %>% 
    gt_edges(text, screen_name, status_id, created_at) %>% 
    gt_nodes() %>% 
    gt_dyn() %>% 
    gt_collect() %>% 
    expect_is("list")
  
  tweets %>% 
    gt_edges(text, screen_name, status_id, created_at) %>% 
    gt_nodes() %>% 
    gt_dyn(5000) %>% 
    gt_collect() %>% 
    expect_is("list")
})

