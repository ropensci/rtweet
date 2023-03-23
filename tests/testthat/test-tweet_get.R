test_that("get_tweet works", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  gt1 <- tweet_get("567053242429734913", parse = FALSE)
  expect_true(inherits(gt1, "list"))
  gt2 <- tweet_get(c("567053242429734913", "567053242429734913"), parse = TRUE)
  expect_s3_class(gt2, "page")
  expect_length(gt2, 3)
  gt2_parsed <- tweet_get(c("567053242429734913", "567053242429734913"),
                          parse = TRUE)
  expect_s3_class(gt2_parsed, "data.frame")
  expect_equal(dim(gt2_parsed), c(2, 3))
})

test_that("get_tweets works with expansions and fields", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  # From issue #757
  expect_error(
    tweet_get("1615009611186069504",
              expansions = 'attachments.media_keys',
              fields = NA, parse = FALSE))
  expect_error(
    gt4 <- tweet_get("1615009611186069504",
                     expansions = 'attachments.media_keys',
                     fields = set_fields(media = "alt_text", NULL, NULL, NULL, NULL, NULL),
                     parse = FALSE), NA)
  expect_equal(gt4[[1]]$includes$media$alt_text, "sort 893")
  expect_error(
    tweet_get("1615009611186069504",
              fields = set_fields(media = "alt_text", NULL, NULL, NULL, NULL, NULL),
              parse = FALSE))
  expect_error(
    tweet_get("1615009611186069504",
              expansions = 'attachments.media_keys',
              parse = FALSE), NA)
})
