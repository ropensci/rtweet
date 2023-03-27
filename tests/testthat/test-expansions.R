test_that("check_expansions works", {
  # Not equal
  expect_error(check_expansions("a", "b"))
  # Two not present
  expect_error(check_expansions(c("a", "c"), "b"))
  # Present and not present
  expect_error(check_expansions(c("a", "b"), "b"))
  # Present
  expect_equal(check_expansions("a", "a"), "a")

  # NULL returns NULL
  expect_equal(check_expansions(NULL, "a"), NULL)
  # Empty vector returns NULL
  expect_equal(check_expansions(c(), "a"), NULL)
  # Empty list returns NULL
  expect_equal(check_expansions(list(), "a"), NULL)
  expect_equal(check_expansions(NA, "a"), NULL)
})

test_that("set_expansions works", {
  expect_error(set_expansions(), NA)
  expect_error(set_expansions(NULL), NA)
  expect_null(set_expansions(NULL, NULL, NULL))
  expect_null(set_expansions(NULL, NULL, c()))
  expect_error(set_expansions("a"),
               "These extensions are not allowed: a",
               fixed = TRUE)
  expect_error(set_expansions(user = "a"),
               "These extensions are not allowed: a",
               fixed = TRUE)
})

test_that("expansions and fields work together", {
  expect_error(expansions_for_fields(NULL, fields = set_fields()),
               "Missing expansions for the fields provided.")
  expect_true(expansions_for_fields(NULL,
                                    fields = set_fields(poll = NULL,
                                                        tweet = NULL,
                                                        media = NULL,
                                                        place = NULL,
                                                        user = NULL)))
  expect_error(expansions_for_fields(NULL,
                                    fields = set_fields(poll = NULL,
                                                        tweet = NULL,
                                                        place = NULL,
                                                        user = NULL)),
               "attachments.media_keys", fixed = TRUE)
  expect_error(expansions_for_fields(NULL,
                                    fields = set_fields(poll = NULL,
                                                        tweet = NULL,
                                                        media = NULL,
                                                        user = NULL)),
               "geo.place_id", fixed = TRUE)
  expect_error(expansions_for_fields(NULL,
                                    fields = set_fields(tweet = NULL,
                                                        place = NULL,
                                                        media = NULL,
                                                        user = NULL)),
               "attachments.poll_ids", fixed = TRUE)
  expect_error(expansions_for_fields(NULL,
                                    fields = set_fields(poll = NULL,
                                                        tweet = NULL,
                                                        place = NULL,
                                                        media = NULL)),
               "Add at least one of", fixed = TRUE)
  expect_error(
    expansions_for_fields(
      expansion = 'attachments.media_keys',
      fields = set_fields(media = "alt_text", NULL, NULL, NULL, NULL)),
    NA)
})
