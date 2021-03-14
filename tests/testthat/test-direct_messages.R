test_that("direct_messages works", {
  dm <- direct_messages()
  expect_type(dm, "list")
  expect_length(dm, 3L)
})

test_that("direct_messages functions", {
  dms <- direct_messages()
  expect_true(is.list(dms))
  expect_true(is.data.frame(dms$events))
  expect_error(direct_messages_received())
  expect_error(direct_messages_sent())
})

test_that("old functions give informative errors", {
  expect_error(direct_messages_received(), "no longer exists")
  expect_error(direct_messages_sent(), "no longer exists")
})
