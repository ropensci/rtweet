test_that("can retrieve multiple users", {
  skip_if_offline()
  users <- c("hadleywickham", "jennybryan")
  vcr::use_cassette("get_favorites1", {
    out <- get_favorites(users, n = 20)
  })
  expect_s3_class(out, "data.frame")
  expect_true(is.character(out$created_at))
  expect_equal(unique(out$favorited_by), users)
})

test_that("get_favorites returns tweets data", {
  skip_if_offline()
  n <- 100
  vcr::use_cassette("get_favorites2", {
    x <- get_favorites("kearneymw", n = n)
  })
  
  expect_equal(is.data.frame(x), TRUE)
  expect_named(x)
  expect_true("id" %in% names(x))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 15)
  expect_true(is.data.frame(users_data(x)))
  #expect_gt(nrow(users_data(x)), 0)
  #expect_gt(ncol(users_data(x)), 15)
  #expect_named(users_data(x))
})

test_that("favorites warns on a locked user", {
  skip_if_offline()
  vcr::use_cassette("get_favorites3", {
    expect_warning(gtf <- get_favorites("515880511"),
                   "Skipping unauthorized account: 515880511")
  })
})
# unauthorized 

test_that("favorites warns on a banned user", {
  skip_if_offline()
  vcr::use_cassette("get_favorites4", {
    expect_warning(gtf <- get_favorites("realdonaldtrump"),
                   "Skipping unauthorized account: realdonaldtrump")
  })
})

test_that("favorites warns on a locked user but continues", {
  skip_if_offline()
  vcr::use_cassette("get_favorites5", {
    expect_warning(gtf <- get_favorites(c("515880511" = "bhs928",
                                          "no_idea" = "Lluis_Revilla")),
                   "Skipping unauthorized account: bhs928")
  })
  expect_gt(nrow(gtf), 2)
  expect_true(all(gtf$favorited_by == "Lluis_Revilla"))
})
