test_that("direct_messages works", {
  dm <- direct_messages()
  expect_type(dm, "list")
  expect_length(dm, 2L)
})

test_that("old functions give informative errors", {
  expect_error(direct_messages_received(), "no longer exists")
  expect_error(direct_messages_sent(), "no longer exists")
})
