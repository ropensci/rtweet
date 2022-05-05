test_that("get_friends returns data frame with ids", {
  
  vcr::use_cassette("friends1", {
    f <- get_friends("kearneymw")
  })
  expect_true(is.data.frame(f))
  ##expect_true(is.character(f[["ids"]]))
  expect_gt(nrow(f), 200)
})

test_that("friendships returns data", {
  
  vcr::use_cassette("friends2", {
    x <- my_friendships("kearneymw")
  })
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("screen_name" %in% names(x))
  vcr::use_cassette("friends3", {
    x <- lookup_friendships("kearneymw", c("realdonaldtrump", "cstonehoops"))
  })
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("relationship" %in% names(x))
})

test_that("get_friends works", {
  
  vcr::use_cassette("friends4", {
    djt <- get_friends("ropensci")
  })
  expect_s3_class(djt, "data.frame")
  expect_gt(nrow(djt), 50)
})

test_that("lookup_friendships works", {
  
  vcr::use_cassette("friends5", {
    lf <- lookup_friendships("hadley", "Lluis_Revilla")
  })
  expect_s3_class(lf, "data.frame")
})

test_that("my_friendships works", {
  
  vcr::use_cassette("friends6", {
    mf <- my_friendships("hadley")
  })
  expect_s3_class(mf, "data.frame")
})

test_that("n = Inf works", {
  
  vcr::use_cassette("friends7", {
    mf <- get_friends("SmallBuStudio", n = Inf)
  })
  expect_s3_class(mf, "data.frame")
})

vcr::use_cassette("friends8", {
  test_that("n = Inf works", {
    expect_warning(
      expect_warning(
        gf <- rtweet::get_friends(c("fdrennan", "Lluis_Revilla")),
        "Skipping unauthorized account: fdrennan")
      )
    expect_s3_class(gf, "data.frame")
    expect_true(ncol(gf) == 2)
    expect_true(all(is.na(gf$to_id)))
  })
})
