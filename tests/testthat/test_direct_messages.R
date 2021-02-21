context("direct_messages")

test_that("direct_messages functions", {
  skip_on_cran()

  dms <- direct_messages()
  expect_true(is.list(dms))
  expect_true(is.data.frame(dms$events))
  expect_error(direct_messages_received())
  expect_error(direct_messages_sent())
})
