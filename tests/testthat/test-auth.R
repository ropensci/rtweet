test_that("bearer token doesn't accidentally expose secrets", {
  expect_snapshot(
    rtweet_app("abc")
  )
})

test_that("can set and reset auth", {

  auth <- auth_get()
  old <- auth_as(rtweet_app("abc"))
  expect_equal(old, auth)
  auth_as(auth)
})

test_that("can save and reload auth", {
  withr::local_options("rtweet:::config_dir" = tempfile())

  auth1 <- rtweet_app("abc")
  suppressMessages({
    auth_save(auth1, "test")
    auth2 <- find_auth("test")
  })
  expect_equal(auth1, auth2, ignore_attr = TRUE)
})

test_that("find auth errors politely", {
  withr::local_options("rtweet:::config_dir" = tempfile())
  expect_snapshot(error = TRUE, {
    find_auth(1:10)
    find_auth("not-present")
  })
})

test_that("default_cached_auth() handles 0, 1, and n saved", {
  withr::local_options("rtweet:::config_dir" = tempfile())
  auth <- rtweet_app("abc")

  # Error if no default auth set up
  expect_snapshot(default_cached_auth(), error = TRUE)

  # Listing options if available
  suppressMessages(auth_save(auth, "test1"))
  suppressMessages(auth_save(auth, "test2"))
  expect_snapshot(default_cached_auth(), error = TRUE)

  # Uses default if present
  suppressMessages(auth_save(auth, "default"))
  expect_equal(default_cached_auth(), auth)
})

test_that("is_developing works", {
  expect_true(is_developing())
})

test_that("rtweet_user works", {
  skip("requires manual testing")
  # Avoid saving it but check that it is redirected in the browser.
  withr::local_options("rtweet:::config_dir" = tempfile())
  expect_error(rtweet_user())
})

test_that("rtweet_bot works", {
  skip("requires manual testing")
  withr::local_options("rtweet:::config_dir" = tempfile())
  expect_error(rtweet_bot(), NA)
})

test_that("rtweet_oauth2 works", {
  skip("requires manual testing")
  # Avoid saving it but check that it is redirected in the browser.
  withr::local_options("rtweet:::config_dir" = tempfile())
  expect_error(b <- rtweet_client(app = "academic_dev"))
  client_as(b)
  expect_error(rtweet_oauth2(), NA)
})
