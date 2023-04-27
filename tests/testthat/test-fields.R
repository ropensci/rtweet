test_that("set_fields works", {
  expect_null(set_fields(NULL, NULL, NULL, NULL, NULL, NULL))
  expect_error(set_fields(NULL, NULL, NULL, NULL, NULL, list = "a"))
  expect_error(set_fields(NULL, NULL, NULL, NULL, "created_at", list = "a"),
               "Invalid")
  expect_equal(set_fields(NULL, NULL, NULL, NULL, "created_at",
                          list = list_fields),
               list(user.fields = "created_at",
                    list.fields = list_fields))
  expect_type(set_fields(NULL, NULL, NULL, NULL), "list")
  expect_error(set_fields(NULL, NULL, NULL, NULL, 1), "characters")
  expect_error(set_fields(NULL, NULL, NULL, NULL, "a"), "Invalid")
})

test_that("check_field_helper works", {
  expect_length(check_field_helper(list(media = "a"), "b", "media"), 1L)
  expect_length(check_field_helper(list(media = "a"), c("b", "c"), "media"), 1L)
  expect_length(check_field_helper(list(media = "b"), c("b", "c"), "media"), 0L)
  expect_length(check_field_helper(list(media = c("b", "c")), c("b", "c"), "media"), 0L)
  expect_length(check_field_helper(list(media = c("b", "c")), c("b", "c"), "test"), 0L)
  expect_length(check_field_helper(list(media = c("b", "c")), NULL, "media"), 1L)
  expect_equal(check_field_helper(list(media = c("b", "c")), NULL, "media"),
               c(x = "Invalid media.\n"))

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
    media = c(
      "duration_ms", "height", "media_key",
      "preview_image_url", "type", "url", "width",
      "public_metrics", "alt_text", "variants"
    ),
    place = c("contained_within", "country", "country_code", "full_name", "geo", "id", "name", "place_type"),
    poll = c("duration_minutes", "end_datetime", "id", "options", "voting_status"),
    tweet = c("attachments", "author_id", "context_annotations", "conversation_id", "created_at", "edit_controls", "entities", "geo", "id", "in_reply_to_user_id", "lang", "public_metrics", "possibly_sensitive", "referenced_tweets", "reply_settings", "source", "text", "withheld"),
    user = c("created_at", "description", "entities", "id", "location", "name", "pinned_tweet_id", "profile_image_url", "protected", "public_metrics", "url", "username", "verified", "withheld"),
    metrics = NULL
  )
  expect_null(out_null)

  # If already provided just check
  out <- check_fields(set_fields())
  expect_equal(out, set_fields())

  out_3 <- check_fields(list(), media = "a", place = "b", poll = "c",
                        tweet = "d", user = "e", metrics = "f")
  expect_equal(out_3, NULL)
  out_3 <- check_fields(NA, media = "a", place = "b", poll = "c", tweet = "d",
                        user = "e", metrics = "f")
  expect_equal(out_3, NULL)
  out_4 <- check_fields(c(), media = "a", place = "b", poll = "c", tweet = "d",
                        user = "e", metrics = "f")
  expect_equal(out_4, NULL)

  out_5 <- check_fields(list(media.fields  = "a"), media = "a", place = "b",
                        poll = "c", tweet = "d", user = "e", metrics = "f")
  expect_equal(out_5, list(media.fields  = "a"))
})
