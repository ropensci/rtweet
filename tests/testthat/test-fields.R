test_that("check_field_helper works", {
  expect_length(check_field_helper(list(media = "a"), "b", "media"), 1L)
  expect_length(check_field_helper(list(media = "a"), c("b", "c"), "media"), 1L)
  expect_length(check_field_helper(list(media = "b"), c("b", "c"), "media"), 0L)
  expect_length(check_field_helper(list(media = c("b", "c")), c("b", "c"), "media"), 0L)
  expect_length(check_field_helper(list(media = c("b", "c")), c("b", "c"), "test"), 0L)
  expect_length(check_field_helper(list(media = c("b", "c")), NULL, "media"), 1L)
  expect_equal(check_field_helper(list(media = c("b", "c")), NULL, "media"), "No media field allowed")

  # NULL returns the allowed fields
  expect_equal(check_field_helper(NULL, "a", "media"), NULL)
  # Empty vector returns allowed
  expect_equal(check_field_helper(c(),  "a", "media"), NULL)
  # Empty list returns empty expansions
  expect_equal(check_field_helper(list(), "a", "media"), NULL)
})

test_that("check_fields works", {
  # If null allow all the fields
  out_null <- check_fields(NULL,
    media_fields = c(
      "duration_ms", "height", "media_key",
      "preview_image_url", "type", "url", "width",
      "public_metrics", "alt_text", "variants"
    ),
    place_fields = c("contained_within", "country", "country_code", "full_name", "geo", "id", "name", "place_type"),
    poll_fields = c("duration_minutes", "end_datetime", "id", "options", "voting_status"),
    tweet_fields = c("attachments", "author_id", "context_annotations", "conversation_id", "created_at", "edit_controls", "entities", "geo", "id", "in_reply_to_user_id", "lang", "public_metrics", "possibly_sensitive", "referenced_tweets", "reply_settings", "source", "text", "withheld"),
    user_fields = c("created_at", "description", "entities", "id", "location", "name", "pinned_tweet_id", "profile_image_url", "protected", "public_metrics", "url", "username", "verified", "withheld"),
    metrics_fields = NULL
  )
  expect_named(out_null, c("media.fields", "place.fields", "poll.fields", "tweet.fields",
                           "user.fields"))
  # If already provided just check
  out <- check_fields(out_null,
                      place_fields = c("contained_within", "country", "country_code", "full_name", "geo", "id", "name", "place_type"),
                      poll_fields = c("duration_minutes", "end_datetime", "id", "options", "voting_status"),
                      tweet_fields = c("attachments", "author_id", "context_annotations", "conversation_id", "created_at", "edit_controls", "entities", "geo", "id", "in_reply_to_user_id", "lang", "public_metrics", "possibly_sensitive", "referenced_tweets", "reply_settings", "source", "text", "withheld"),
                      user_fields = c("created_at", "description", "entities", "id", "location", "name", "pinned_tweet_id", "profile_image_url", "protected", "public_metrics", "url", "username", "verified", "withheld"),
                      metrics_fields = NULL)
  expect_equal(out, out_null)

  out_3 <- check_fields(list(),
                        media_fields = "a",
                        place_fields = "b",
                        poll_fields = "c",
                        tweet_fields = "d",
                        user_fields = "e",
                        metrics_fields = "f"
  )
  expect_equal(out_3, NULL)
  out_3 <- check_fields(NA,
                        media_fields = "a",
                        place_fields = "b",
                        poll_fields = "c",
                        tweet_fields = "d",
                        user_fields = "e",
                        metrics_fields = "f"
  )
  expect_equal(out_3, NULL)
  out_4 <- check_fields(c(),
                        media_fields = "a",
                        place_fields = "b",
                        poll_fields = "c",
                        tweet_fields = "d",
                        user_fields = "e",
                        metrics_fields = "f"
  )
  expect_equal(out_4, list(media.fields = "a", place.fields = "b",
                           poll.fields = "c", tweet.fields = "d",
                           user.fields = "e", metrics.fields = "f"))

})
