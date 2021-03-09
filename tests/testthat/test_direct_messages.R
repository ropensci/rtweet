test_that("direct_messages functions", {
  dms <- direct_messages()
  expect_true(is.list(dms))
  expect_true(is.data.frame(dms$events))
  expect_error(direct_messages_received())
  expect_error(direct_messages_sent())
})
