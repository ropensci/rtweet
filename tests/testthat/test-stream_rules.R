test_that("Request all current rule ", {
  skip_if_offline()
  if (!"RTAA" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("RTAA")
  expect_error(rt <- stream_add_rule(NULL), NA)
  expect_equal(rt$result_count, 0)
})

test_that("Add a streaming rule ", {
  skip_if_offline()
  if (!"RTAA" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("RTAA")
  expect_error(rt0 <- stream_add_rule(list(value = "testing rules", tag = "ts")), NA)
  expect_equal(rt0$created, 1)
  expect_equal(rt0$valid, 1)

  # Clean up
  stream_rm_rule(attr(rt0, "rules")$id)
})

test_that("Handle adding duplicate streaming rules ", {
  skip_if_offline()
  if (!"RTAA" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("RTAA")
  rt0 <- stream_add_rule(list(value = "testing rules", tag = "ts"))
  expect_warning(rt <- stream_add_rule(list(value = "testing rules", tag = "ts")))

  # Clean up
  expect_equal(stream_rm_rule(rt$errors$id)$deleted, 1)
})


test_that("Handle multiple streaming rules ", {
  skip_if_offline()
  if (!"RTAA" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("RTAA")
  tags2 <- stream_add_rule(list(
    list(value = "testing rules rtweet1", tag = "tsrt"),
    list(value = "testing rules rtweet2", tag = "tsrt2"))
  )
  expect_equal(tags2$created, 2)
  # Clean up
  expect_equal(stream_rm_rule(attr(tags2, "rules")$id)$deleted, 2)

})
