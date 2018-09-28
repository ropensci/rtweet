context("direct_messages")

test_that("direct_messages functions", {
  skip_on_cran()

  token <- readRDS("twitter_tokens")
  dms <- direct_messages(token = token)
  expect_true(is.list(dms))
  expect_true(is.data.frame(dms$events))
  expect_error(direct_messages_received(token = token))
  expect_error(direct_messages_sent(token = token))
})
