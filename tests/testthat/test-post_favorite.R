test_that("post_favorite works", {
  rt <- search_tweets("#rstats", n = 1)
  post_favorite(rt$status_id)
  post_favorite(rt$status_id, destroy = TRUE)
})
