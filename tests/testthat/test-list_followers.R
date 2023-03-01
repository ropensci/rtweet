test_that("list_followers works", {
  testing_with_authentication("bearer_testing_app")
  lf <- list_followers("1150793074420998146")
  expect_s3_class(lf, "data.frame")
  expect_true(all(c("id", "name", "username") %in% colnames(lf)))
})
