test_that("search_fullarchive works", {
  # Use app with a premium account that matches the env_names
  if (!"bearer_testing_app" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("bearer_testing_app")
  expect_snapshot(
    df <- search_fullarchive( q = '#covid place:UK OR place:GB OR place:"United Kindom"',
                              n = 20, env_name = 'fullArchive',
                              fromDate = "201810010000"))
  expect_equal(nrow(df), 20)
})

test_that("search_fullarchive queries bigger than page size work", {
  # Use app with a premium account that matches the env_names
  if (!"bearer_testing_app" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("bearer_testing_app")
  expect_snapshot(
    df <- search_fullarchive( q = '#covid place:UK OR place:GB OR place:"United Kindom"',
                              n = 20, env_name = 'fullArchive',
                              fromDate = "201810010000"))
  expect_equal(nrow(df), 20)
})

test_that("search_fullarchive does not return duplicate tweets", {
  # Use app with a premium account that matches the env_names
  if (!"bearer_testing_app" %in% auth_list()) {
    skip("Requires different authentication")
  }
  auth_as("bearer_testing_app")
  expect_snapshot(
    df <- search_fullarchive(q="#halalan22", n = 450,
                                       env_name = "fullArchive",
                                       fromDate = "202202080000", toDate = "202205100000",
                                       parse = TRUE)
  )
  expect_gt(nrow(df), 450)
})
