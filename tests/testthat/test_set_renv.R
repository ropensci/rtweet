context("set_renv")

test_that("set_renv", {
  skip_on_cran()

  rtweet:::set_renv(TEST_PAT = "asdf")
  expect_equal(Sys.getenv("TEST_PAT"), "asdf")
})
