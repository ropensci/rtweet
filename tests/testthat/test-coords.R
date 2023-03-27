test_that("lookup_coords returns coords data", {

  kcmo <- lookup_coords("kansas city, mo")
  expect_gt(cor(kcmo$point, c(39.0997, 94.5786)), 0.9)
  tor <- lookup_coords("toronto canada")
  expect_gt(cor(tor$point, c(43.6532, 79.3832)), 0.9)

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
    vcr::use_cassette("lookup_coords5", {
      x <- lookup_coords("New York, NY")
    })
    expect_equal(is.list(x), TRUE)
    expect_named(x)
    expect_true("box" %in% names(x))
  }
  Sys.setenv(GOOGLE_KEY = gmk)
  Sys.setenv(GOOGLE_MAPS_KEY = "")
  if (!is.null(gmk) && !identical(gmk, "")) {
    vcr::use_cassette("lookup_coords6", {
      x <- lookup_coords("New York, NY")
    })
    expect_equal(is.list(x), TRUE)
    expect_named(x)
    expect_true("box" %in% names(x))
  }
  e <- names(Sys.getenv())
  g <- grep("google|gmap", e, ignore.case = TRUE, value = TRUE)
  if (length(g) > 0) {
    ng <- as.list(rep("", length(g)))
    names(ng) <- g
    do.call(Sys.setenv, ng)
  }
  expect_error(lookup_coords("London, UK"))
})

