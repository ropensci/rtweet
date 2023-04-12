test_that("enlist works well", {
  # In case of a data.frame
  tweet <- list(a = structure(list(a = 1:2, b = c("a", "b")),
                              class = "data.frame", row.names = c(NA, -2L)),
                id = "1638199517878140929",
                d = 1)
  # Transforms
  expect_true(lengths(tweet["a"]) != lengths(enlist(tweet)["a"]))
  expect_true(lengths(enlist(tweet)["a"]) == 1)

  # Same but for a vector
  tweet$a <- 1:2
  expect_true(lengths(tweet["a"]) != lengths(enlist(tweet)["a"]))
  expect_true(lengths(enlist(tweet)["a"]) == 1)
})

test_that("rbinding nested structures", {
  tweet <- list(a = structure(list(a = 1:2, b = c("a", "b")),
                              class = "data.frame", row.names = c(NA, -2L)),
                id = "1638199517878140929",
                d = 1)
  b <- do.call(rbind, list(list2DF(enlist(tweet)), list2DF(enlist(tweet))))
  expect_s3_class(b, "data.frame")
  expect_equal(ncol(b), 3)
  expect_length(vapply(b, class, character(1L)), ncol(b))

  tweet$a <- c(1:2)
  b <- do.call(rbind, list(list2DF(enlist(tweet)), list2DF(enlist(tweet))))
  expect_s3_class(b, "data.frame")
  expect_equal(ncol(b), 3)
  expect_length(vapply(b, class, character(1L)), ncol(b))
})

test_that("parse with multiple edits", {
  skip_on_cran()
  testing_with_authentication("bearer_testing_app")
  out <- tweet_get("1638199517878140929")
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) == 1)
})
