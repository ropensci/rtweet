test_that("post_message works", {
  rt <- expect_message(
    post_message(paste("Testing", Sys.time()), user = "Lluis_Revilla"),
    "your DM has been posted!"
  )
  expect_equal(httr::status_code(rt), 200L)
})
