test_that("post_tweet works", {
  skip_if_offline()
  skip_on_cran()
  
  rt <- expect_message(post_tweet("test"), "your tweet has been posted!")
  crt <- httr::content(rt)
  rt <- post_destroy(as.character(crt$id))
})
