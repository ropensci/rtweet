test_that("lookup_friendships works", {
  lf <- lookup_friendships("hadley", "Lluis_Revilla")
  expect_s3_class(lf, "data.frame")
})
