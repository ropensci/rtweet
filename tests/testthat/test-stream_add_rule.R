test_that("Handle add duplicate stream rule ", {
  auth_as("RTAA")
  rt <- stream_add_rule(list(value = "testing rules", tag = "ts"))
  rt <- stream_add_rule(list(value = "testing rules", tag = "ts"))
})

vcr::use_cassette("stream_add_rule_1", {
  test_that("Request all current rule ", {
    skip_if_offline()
    auth_as("RTAA")
    expect_error(rt <- stream_add_rule(NULL), NA)
  })
})
