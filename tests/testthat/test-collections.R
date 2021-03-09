test_that("lookup_collections returns tweets data", {
  x <- lookup_collections("custom-539487832448843776")
  expect_equal(is.list(x), TRUE)
  expect_named(x)
  expect_true(all(c("response", "objects") %in% names(x)))
})

test_that("get_collections returns a data frame", {
  out <- get_collections(user = "cnn")
  expect_type(out, "list")
  
  out <- get_collections(status_id = "925172982313570306")
  expect_type(out, "list")
})
