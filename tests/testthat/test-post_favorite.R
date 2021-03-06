test_that("post_favorite works", {
  rt <- search_tweets("#rstats", n = 1)
  expect_equal(httr::status_code(post_favorite(rt$status_id)), 200)
  expect_equal(httr::status_code(post_favorite(rt$status_id, destroy = TRUE)), 200)
})
