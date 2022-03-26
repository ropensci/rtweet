test_that("direct_messages works", {
  
  vcr::use_cassette("direct_messages", {
    dm <- direct_messages(n = 1)[[1]]
  })
  expect_type(dm, "list")
  expect_length(dm, 1L)
  expect_named(dm, "events")
  
  # Usually a data.frame but if no message it can be an empty list
  expect_true(is.list(dm$events)) 
})

test_that("old functions give informative errors", {
  expect_error(direct_messages_received(), "no longer exists")
  expect_error(direct_messages_sent(), "no longer exists")
})
