test_that("get_token() and get_tokens() are deprecated", {
  expect_snapshot({
    . <- get_token()
    . <- get_tokens()
  })
})

test_that("create_token is deprecated", {

  path <- tempfile()
  withr::local_options("rtweet:::config_dir" = path)
  expect_snapshot(token <- suppressMessages(create_token("my-app", "x", "x", "y", "y")))

  # still sets as default
  expect_equal(auth_get(), token)

  # and saves in config dir
  expect_equal(dir(path), "create_token.rds")
})
