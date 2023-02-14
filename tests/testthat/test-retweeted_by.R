test_that("retweeted_by works", {
  testing_with_authentication("bearer_testing_app")
  # Pinned tweets where deleted so there will be errors
  rb <- retweeted_by("567053242429734913", parse = FALSE, expansions = NULL,
                     fields = NULL)


})
