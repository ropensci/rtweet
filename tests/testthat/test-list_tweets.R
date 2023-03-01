test_that("list_tweets works", {
  testing_with_authentication("bearer_testing_app")
  lt <- list_tweets("1150793074420998146")
  expect_s3_class(lt, "data.frame")
  expect_true(all(c("edit_history_tweet_ids", "id", "text") %in% colnames(lt)))
})
