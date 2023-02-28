test_that("Request all current rule ", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  rt <- expect_error(stream_add_rule(NULL), NA)
  expect_equal(rt$result_count, 0)
})

test_that("Add a streaming rule ", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  stream_rm_rule(ids(stream_add_rule(NULL)))
  expect_error(rt0 <- stream_add_rule(list(value = "testing rules", tag = "ts")), NA)
  expect_equal(rt0$created, 1)
  expect_equal(rt0$valid, 1)

  # Clean up
  stream_rm_rule(attr(rt0, "rules")$id)
})

test_that("Handle adding duplicate streaming rules ", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  stream_rm_rule(ids(stream_add_rule(NULL)))
  rt0 <- stream_add_rule(list(value = "testing rules", tag = "ts"))
  expect_error(rt <- stream_add_rule(list(value = "testing rules", tag = "ts")))

  # Clean up
  expect_equal(stream_rm_rule(rt$errors$id)$deleted, 1)
})


test_that("Handle multiple streaming rules ", {
  skip_if_offline()
  testing_with_authentication("bearer_testing_app")
  stream_rm_rule(ids(stream_add_rule(NULL)))
  tags2 <- stream_add_rule(list(
    list(value = "testing rules rtweet1", tag = "tsrt"),
    list(value = "testing rules rtweet2", tag = "tsrt2"))
  )
  expect_equal(tags2$created, 2)
  # Clean up
  expect_equal(stream_rm_rule(attr(tags2, "rules")$id)$deleted, 2)
})
