test_that("search_fullarchive works", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  skip("requires manual testing")
  df <- search_fullarchive( q = '#covid place:UK OR place:GB OR place:"United Kindom"',
                            n = 20, env_name = 'fullArchive',
                            fromDate = "201810010000",
                            token = auth_as("bearer_testing_app"))
  expect_equal(nrow(df), 20)
})

test_that("search_fullarchive queries bigger than page size work", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  skip("requires manual testing")
  df <- search_fullarchive( q = '#covid place:UK OR place:GB OR place:"United Kindom"',
                            n = 20, env_name = 'fullArchive',
                            fromDate = "201810010000",
                            token = auth_as("bearer_testing_app"))
  expect_equal(nrow(df), 20)
})
