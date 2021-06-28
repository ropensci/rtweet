coln <- c("created_at", "id", "id_str", "full_text", "truncated", "display_text_range", 
          "entities", "source", "in_reply_to_status_id", "in_reply_to_status_id_str", 
          "in_reply_to_user_id", "in_reply_to_user_id_str", "in_reply_to_screen_name", 
          "geo", "coordinates", "place", "contributors", "retweeted_status", 
          "is_quote_status", "quoted_status_id", "quoted_status_id_str", 
          "quoted_status_permalink", "retweet_count", "favorite_count", 
          "favorited", "retweeted", "lang", "possibly_sensitive", "quoted_status", 
          "text")

test_that("lookup_statuses returns users data", {
  ids <- c("558115838503690243", "760182486005583872", "776053079540166657")
  x <- lookup_tweets(ids)
  expect_true(all(coln %in% colnames(x))) 
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 2) # 558115838503690243 was deleted
})


test_that("lookup status no retweet, no reply no quote", {
  status <- lookup_tweets("1333789433288540160")
  expect_true(all(coln %in% colnames(status))) 
})

test_that("lookup on reply, no quote no retweet", {
  reply <- lookup_tweets("1333789435482161153")
  expect_true(all(coln %in% colnames(reply))) 
})

test_that("lookup on retweet quotting", {
  retweet_quoted <- lookup_tweets("1390610121743556609")
  expect_true(all(coln %in% colnames(retweet_quoted))) 
})


test_that("lookup on retweet", {
  retweet <- lookup_tweets("1390785143615467524")
  expect_true(all(coln %in% colnames(retweet))) 
})


test_that("lookup on users without tweets, #574", {
  lu <- lookup_users("994659707766833153")
  td <- tweets_data(lu)
  expect_equal(nrow(td), 1)
})
