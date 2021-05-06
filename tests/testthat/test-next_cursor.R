# cursor ------------------------------------------------------------------

test_that("next_cursor works with character and data frames", {
  expect_equal(next_cursor("abc"), "abc")
  
  df <- data.frame(x = 1:2)
  attr(df, "rtweet_cursor") <- "def"
  expect_equal(next_cursor(df), "def")
})

test_that("next_cursor generates informative errors", {
  expect_snapshot(error = TRUE, {
    next_cursor(letters)
    next_cursor(mtcars)
  })
})


# id ----------------------------------------------------------------------

test_that("max_id and since_id work in bit64", {
  expect_equal(max_id("0"), "-1")
  expect_equal(max_id(c("50", "100")), "49")
  
  expect_equal(since_id(c("50", "100")), "100")
})

test_that("max_id and since_id work with data frames", {
  df <- data.frame(id = "123", stringsAsFactors = FALSE)
  expect_equal(max_id(df), "122")
  expect_equal(since_id(df), "123")
})

test_that("max_id and since_id work with data frames and factors", {
  df <- data.frame(id = "123")
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
