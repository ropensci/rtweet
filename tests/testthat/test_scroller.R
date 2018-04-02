
test_that("unique_id returns correct counts", {
  skip_on_cran()

  x <- list(data.frame(user_id = "123"), data.frame(user_id = "234"))

  expect_equal(unique_id_count(x), 2)
})

