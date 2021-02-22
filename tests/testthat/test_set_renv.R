context("set_renv")

test_that("set_renv", {
  skip_on_cran()
  skip_if_offline()

  rtweet:::set_renv(TEST_PAT = "asdf")
  expect_equal(Sys.getenv("TEST_PAT"), "asdf")
})
