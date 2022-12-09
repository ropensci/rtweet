test_that("lookup_users returns users data", {

  vcr::use_cassette("lookup_users1", {
    x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))
  })
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 4)
})

test_that("lookup_users works", {

  users <- c(
    "potus", "hillaryclinton", "realdonaldtrump",
    "fivethirtyeight", "cnn", "espn", "twitter"
  )
  vcr::use_cassette("lookup_users2", {
    usr_df <- lookup_users(users)
  })
  expect_s3_class(usr_df, "data.frame")
  expect_equal(nrow(usr_df), 6)
  expect_equal(ncol(usr_df), 23)
})


test_that("users with same information, #654", {

  vcr::use_cassette("lookup_users3", {
    a <- lookup_users("alexpghayes")
  })
  vcr::use_cassette("lookup_users4", {
    d <- lookup_users("Corey_Yanofsky")
  })

  expect_length(setdiff(colnames(a), colnames(d)), 0)
})

test_that("Users with date formatting, #653", {

  vcr::use_cassette("lookup_users5", {
    x <- lookup_users("alexpghayes")
  })
  expect_s3_class(x$created_at, "POSIXct")
})

test_that("lookup_users only works with ids", {
  vcr::use_cassette("lookup_users1", {
    x <- lookup_users(c("cnn", "potus", "twitter", "kearneymw"))
  })

  twd <- tweets_data(x)
  expect_error(b <- lookup_tweets(twd), NA)
  expect_s3_class(b, "tweets")

})
