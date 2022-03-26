coln <- c("created_at", "id", "id_str", "full_text", "truncated", "display_text_range", 
          "entities", "source", "in_reply_to_status_id", "in_reply_to_status_id_str", 
          "in_reply_to_user_id", "in_reply_to_user_id_str", "in_reply_to_screen_name", 
          "geo", "coordinates", "place", "contributors", "retweeted_status", 
          "is_quote_status", "quoted_status_id", "quoted_status_id_str", 
          "quoted_status_permalink", "retweet_count", "favorite_count", 
          "favorited", "retweeted", "lang", "possibly_sensitive", "quoted_status", 
          "text")

test_that("lookup_statuses returns users data", {
  skip_if_offline()
  ids <- c("558115838503690243", "760182486005583872", "776053079540166657")
  vcr::use_cassette("ids1", {
    x <- lookup_tweets(ids)
  })
  expect_true(all(coln %in% colnames(x))) 
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 2) # 558115838503690243 was deleted
})


test_that("lookup status no retweet, no reply no quote", {
  skip_if_offline()
  vcr::use_cassette("ids2", {
    status <- lookup_tweets("1333789433288540160")
  })
  expect_true(all(coln %in% colnames(status))) 
})

test_that("lookup on reply, no quote no retweet", {
  skip_if_offline()
  vcr::use_cassette("ids3", {
    reply <- lookup_tweets("1333789435482161153")
  })
  expect_true(all(coln %in% colnames(reply))) 
})

test_that("lookup on retweet quotting", {
  skip_if_offline()
  vcr::use_cassette("ids4", {
    retweet_quoted <- lookup_tweets("1390610121743556609")
  })
  expect_true(all(coln %in% colnames(retweet_quoted))) 
})


test_that("lookup on retweet", {
  skip_if_offline()
  vcr::use_cassette("ids5", {
    retweet <- lookup_tweets("1390785143615467524")
  })
  expect_true(all(coln %in% colnames(retweet))) 
})


test_that("lookup on users without tweets, #574", {
  skip_if_offline()
  vcr::use_cassette("ids6", {
    lu <- lookup_users("994659707766833153")
  })
  td <- tweets_data(lu)
  expect_equal(nrow(td), 1)
})

test_that("lookup on users with scopes, #615", {
  skip_if_offline()
  vcr::use_cassette("ids7", {
    lu <- lookup_tweets("1400810492843630598")
  })
  expect_equal(nrow(lu), 1)
})

test_that("Check coordinates on different autoformatting from jsonlite", {
  skip_if_offline()
  vcr::use_cassette("ids8", {
    lu <- lookup_tweets(c("368194158915506176"))
  })
  expect_true(is.list(lu$coordinates) && is.data.frame(lu$coordinates[[1]]))
  expect_equal(nrow(lu), 1)
})


test_that("Check that geo works well,  #648", {
  skip_if_offline()
  vcr::use_cassette("ids9", {
    lu <- lookup_tweets(c("1488182699202383875", "1373362476839022592", "1481348667307180033", 
                          "930475046530936834", "914607458169081858"))
  })
  expect_true(is.list(lu$geo) && !is.data.frame(lu$geo))
})

test_that("Check lookup returns appropiate format,  #657", {
  skip_if_offline()
  vcr::use_cassette("ids10", {
    lu <- lookup_tweets("1488182699202383875")
  })
  expect_s3_class(lu$created_at, "POSIXct")
})
