test_that("my_friendships works", {
  mf <- my_friendships("hadley")
  expect_s3_class(mf, "data.frame")
})
