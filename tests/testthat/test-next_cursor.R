test_that("max_id and since_id work in bit64", {
  expect_equal(max_id("0"), "-1")
  expect_equal(max_id(c("50", "100")), "49")
  
  expect_equal(since_id(c("50", "100")), "100")
})

test_that("max_id and since_id work with data frames", {
  df <- data.frame(status_id = "123")
  expect_equal(max_id(df), "122")
  expect_equal(since_id(df), "123")
})

test_that("max_id and since_id generate informative erorrs", {
  expect_snapshot(error = TRUE, {
    max_id(10)
    max_id(mtcars)
    
    since_id(10)
    since_id(mtcars)
  })
})
