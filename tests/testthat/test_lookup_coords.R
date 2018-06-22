
context("lookup_coords")

test_that("lookup_coords returns coords data", {
  skip_on_cran()

  x <- lookup_coords("usa")
  expect_equal(is.list(x), TRUE)
  expect_named(x)
  expect_true("box" %in% names(x))

  x <- lookup_coords("world")
  expect_equal(is.list(x), TRUE)
  expect_named(x)
  expect_true("box" %in% names(x))

  gmk <- Sys.getenv("GOOGLE_MAPS_KEY")
  if (!is.null(gmk) && !identical(gmk, "")) {
    x <- lookup_coords("New York, NY")
    expect_equal(is.list(x), TRUE)
    expect_named(x)
    expect_true("box" %in% names(x))
  }
  rtweet:::set_renv(GOOGLE_KEY = gmk)
  rtweet:::set_renv(GOOGLE_MAPS_KEY = "")
  if (!is.null(gmk) && !identical(gmk, "")) {
    x <- lookup_coords("New York, NY")
    expect_equal(is.list(x), TRUE)
    expect_named(x)
    expect_true("box" %in% names(x))
  }
  e <- names(Sys.getenv())
  g <- grep("google", e, ignore.case = TRUE, value = TRUE)
  ng <- as.list(rep("", length(g)))
  names(ng) <- g
  do.call(Sys.setenv, ng)
  expect_error(lookup_coords("London, UK"))
})

