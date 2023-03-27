test_that("list_get works", {
  testing_with_authentication("bearer_testing_app")
  lg <- list_get("1306285118877831168")
  expect_s3_class(lg, "data.frame")
})
