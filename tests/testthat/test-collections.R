test_that("collections API has been deprecated", {
  expect_snapshot(error = TRUE, {
    lookup_collections("custom-539487832448843776")
    get_collections(status_id = "925172982313570306")
  })
})
